package rtp

import (
	"encoding/binary"
	"fmt"
	"io"
	"github.com/mudutv/rtp/codecs"
	"github.com/pkg/errors"
	"github.com/mudutv/rtp/utils"
)


// Extension RTP Header extension
type Extension struct {
	id      uint8
	payload []byte
}

// TODO(@kixelated) Remove Header.PayloadOffset and Packet.Raw

// Header represents an RTP packet header
// NOTE: PayloadOffset is populated by Marshal/Unmarshal and should not be modified
type Header struct {
	Version          uint8
	Padding          bool
	Extension        bool
	Marker           bool
	PayloadOffset    int
	PayloadType      uint8
	SequenceNumber   uint16
	Timestamp        uint32
	SSRC             uint32
	CSRC             []uint32
	ExtensionProfile uint16
	Extensions       []Extension
}

//miaobinwei
type BytesExtension struct {
	id uint8
	len uint8
	value []byte
}

// Packet represents an RTP Packet
// NOTE: Raw is populated by Marshal/Unmarshal and should not be modified
type Packet struct {
	Header
	Raw     []byte
	RawLen  int
	Payload []byte
	PayloadLength  int
	PayloadPadding uint8

	//MapBytesExtensions map[uint8]BytesExtension
	PayloadDescriptorHandler codecs.PayloadDescriptor
}

const (
	headerLength            = 4
	versionShift            = 6
	versionMask             = 0x3
	paddingShift            = 5
	paddingMask             = 0x1
	extensionShift          = 4
	extensionMask           = 0x1
	extensionProfileOneByte = 0xBEDE
	extensionProfileTwoByte = 0x1000
	extensionIDReserved     = 0xF
	ccMask                  = 0xF
	markerShift             = 7
	markerMask              = 0x1
	ptMask                  = 0x7F
	seqNumOffset            = 2
	seqNumLength            = 2
	timestampOffset         = 4
	timestampLength         = 4
	ssrcOffset              = 8
	ssrcLength              = 4
	csrcOffset              = 12
	csrcLength              = 4
)

// String helps with debugging by printing packet information in a readable way
func (p Packet) String() string {
	out := "RTP PACKET:\n"

	out += fmt.Sprintf("\tVersion: %v\n", p.Version)
	out += fmt.Sprintf("\tMarker: %v\n", p.Marker)
	out += fmt.Sprintf("\tPayload Type: %d\n", p.PayloadType)
	out += fmt.Sprintf("\tSequence Number: %d\n", p.SequenceNumber)
	out += fmt.Sprintf("\tTimestamp: %d\n", p.Timestamp)
	out += fmt.Sprintf("\tSSRC: %d (%x)\n", p.SSRC, p.SSRC)
	out += fmt.Sprintf("\tPayload Length: %d\n", len(p.Payload))

	return out
}

// Unmarshal parses the passed byte slice and stores the result in the Header this method is called upon
func (h *Header) Unmarshal(rawPacket []byte) error {
	if len(rawPacket) < headerLength {
		return fmt.Errorf("RTP header size insufficient; %d < %d", len(rawPacket), headerLength)
	}

	/*
	 *  0                   1                   2                   3
	 *  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
	 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	 * |V=2|P|X|  CC   |M|     PT      |       sequence number         |
	 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	 * |                           timestamp                           |
	 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	 * |           synchronization source (SSRC) identifier            |
	 * +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
	 * |            contributing source (CSRC) identifiers             |
	 * |                             ....                              |
	 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	 */

	h.Version = rawPacket[0] >> versionShift & versionMask
	h.Padding = (rawPacket[0] >> paddingShift & paddingMask) > 0
	h.Extension = (rawPacket[0] >> extensionShift & extensionMask) > 0
	h.CSRC = make([]uint32, rawPacket[0]&ccMask)

	currOffset := csrcOffset + (len(h.CSRC) * csrcLength)
	if len(rawPacket) < currOffset {
		return fmt.Errorf("size %d < %d: %w", len(rawPacket), currOffset, errHeaderSizeInsufficient)
	}

	h.Marker = (rawPacket[1] >> markerShift & markerMask) > 0
	h.PayloadType = rawPacket[1] & ptMask

	h.SequenceNumber = binary.BigEndian.Uint16(rawPacket[seqNumOffset : seqNumOffset+seqNumLength])
	h.Timestamp = binary.BigEndian.Uint32(rawPacket[timestampOffset : timestampOffset+timestampLength])
	h.SSRC = binary.BigEndian.Uint32(rawPacket[ssrcOffset : ssrcOffset+ssrcLength])

	for i := range h.CSRC {
		offset := csrcOffset + (i * csrcLength)
		h.CSRC[i] = binary.BigEndian.Uint32(rawPacket[offset:])
	}

	if h.Extension {
		if expected := currOffset + 4; len(rawPacket) < expected {
			return fmt.Errorf("size %d < %d: %w",
				len(rawPacket), expected,
				errHeaderSizeInsufficientForExtension,
			)
		}

		h.ExtensionProfile = binary.BigEndian.Uint16(rawPacket[currOffset:])
		currOffset += 2
		extensionLength := int(binary.BigEndian.Uint16(rawPacket[currOffset:])) * 4
		currOffset += 2

		if expected := currOffset + extensionLength; len(rawPacket) < expected {
			return fmt.Errorf("size %d < %d: %w",
				len(rawPacket), expected,
				errHeaderSizeInsufficientForExtension,
			)
		}

		switch h.ExtensionProfile {
		// RFC 8285 RTP One Byte Header Extension
		case extensionProfileOneByte:
			end := currOffset + extensionLength
			for currOffset < end {
				if rawPacket[currOffset] == 0x00 { // padding
					currOffset++
					continue
				}

				extid := rawPacket[currOffset] >> 4
				len := int(rawPacket[currOffset]&^0xF0 + 1)
				currOffset++

				if extid == extensionIDReserved {
					break
				}

				extension := Extension{id: extid, payload: rawPacket[currOffset : currOffset+len]}
				h.Extensions = append(h.Extensions, extension)
				currOffset += len
			}

		// RFC 8285 RTP Two Byte Header Extension
		case extensionProfileTwoByte:
			end := currOffset + extensionLength
			for currOffset < end {
				if rawPacket[currOffset] == 0x00 { // padding
					currOffset++
					continue
				}

				extid := rawPacket[currOffset]
				currOffset++

				len := int(rawPacket[currOffset])
				currOffset++

				extension := Extension{id: extid, payload: rawPacket[currOffset : currOffset+len]}
				h.Extensions = append(h.Extensions, extension)
				currOffset += len
			}

		default: // RFC3550 Extension
			if len(rawPacket) < currOffset+extensionLength {
				return fmt.Errorf("RTP header size insufficient for extension length; %d < %d", len(rawPacket), currOffset+extensionLength)
			}

			extension := Extension{id: 0, payload: rawPacket[currOffset : currOffset+extensionLength]}
			h.Extensions = append(h.Extensions, extension)
			currOffset += len(h.Extensions[0].payload)
		}
	}

	h.PayloadOffset = currOffset

	return nil
}

// Unmarshal parses the passed byte slice and stores the result in the Packet this method is called upon
func (p *Packet) Unmarshal(rawPacket []byte) error {
	if err := p.Header.Unmarshal(rawPacket); err != nil {
		return err
	}

	p.Payload = rawPacket[p.PayloadOffset:]
	p.PayloadLength = len(p.Payload)
	p.Raw = rawPacket
	p.RawLen = len(p.Raw)

	p.PayloadPadding = 0
	if (p.Padding == true){
		if (p.PayloadLength == 0) {
			return errors.New("padding bit is set but no space for a padding byte, packet discarded")
		}
		p.PayloadPadding = p.Raw[p.RawLen - 1]
		if (0 == p.PayloadPadding){
			return errors.New("padding byte cannot be 0, packet discarded")
		}

		if (p.PayloadLength < int(p.PayloadPadding) ) {
			return errors.New("number of padding octets is greater than available space for payload, packet discarded")
		}
		p.PayloadLength -= int(p.PayloadPadding)
	}
	//p.ParseExtensions()
	return nil
}

// Marshal serializes the header into bytes.
func (h *Header) Marshal() (buf []byte, err error) {
	buf = make([]byte, h.MarshalSize())

	n, err := h.MarshalTo(buf)
	if err != nil {
		return nil, err
	}

	return buf[:n], nil
}

// MarshalTo serializes the header and writes to the buffer.
func (h *Header) MarshalTo(buf []byte) (n int, err error) {
	/*
	 *  0                   1                   2                   3
	 *  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
	 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	 * |V=2|P|X|  CC   |M|     PT      |       sequence number         |
	 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	 * |                           timestamp                           |
	 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	 * |           synchronization source (SSRC) identifier            |
	 * +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
	 * |            contributing source (CSRC) identifiers             |
	 * |                             ....                              |
	 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	 */

	size := h.MarshalSize()
	if size > len(buf) {
		return 0, io.ErrShortBuffer
	}

	// The first byte contains the version, padding bit, extension bit, and csrc size
	buf[0] = (h.Version << versionShift) | uint8(len(h.CSRC))
	if h.Padding {
		buf[0] |= 1 << paddingShift
	}

	if h.Extension {
		buf[0] |= 1 << extensionShift
	}

	// The second byte contains the marker bit and payload type.
	buf[1] = h.PayloadType
	if h.Marker {
		buf[1] |= 1 << markerShift
	}

	binary.BigEndian.PutUint16(buf[2:4], h.SequenceNumber)
	binary.BigEndian.PutUint32(buf[4:8], h.Timestamp)
	binary.BigEndian.PutUint32(buf[8:12], h.SSRC)

	n = 12
	for _, csrc := range h.CSRC {
		binary.BigEndian.PutUint32(buf[n:n+4], csrc)
		n += 4
	}

	if h.Extension {
		extHeaderPos := n
		binary.BigEndian.PutUint16(buf[n+0:n+2], h.ExtensionProfile)
		n += 4
		startExtensionsPos := n

		switch h.ExtensionProfile {
		// RFC 8285 RTP One Byte Header Extension
		case extensionProfileOneByte:
			for _, extension := range h.Extensions {
				buf[n] = extension.id<<4 | (uint8(len(extension.payload)) - 1)
				n++
				n += copy(buf[n:], extension.payload)
			}
		// RFC 8285 RTP Two Byte Header Extension
		case extensionProfileTwoByte:
			for _, extension := range h.Extensions {
				buf[n] = extension.id
				n++
				buf[n] = uint8(len(extension.payload))
				n++
				n += copy(buf[n:], extension.payload)
			}
		default: // RFC3550 Extension
			extlen := len(h.Extensions[0].payload)
			if extlen%4 != 0 {
				//the payload must be in 32-bit words.
				return 0, io.ErrShortBuffer
			}
			n += copy(buf[n:], h.Extensions[0].payload)
		}

		// calculate extensions size and round to 4 bytes boundaries
		extSize := n - startExtensionsPos
		roundedExtSize := ((extSize + 3) / 4) * 4

		binary.BigEndian.PutUint16(buf[extHeaderPos+2:extHeaderPos+4], uint16(roundedExtSize/4))

		// add padding to reach 4 bytes boundaries
		for i := 0; i < roundedExtSize-extSize; i++ {
			buf[n] = 0
			n++
		}
	}

	h.PayloadOffset = n

	return n, nil
}

// MarshalSize returns the size of the header once marshaled.
func (h *Header) MarshalSize() int {
	// NOTE: Be careful to match the MarshalTo() method.
	size := 12 + (len(h.CSRC) * csrcLength)

	if h.Extension {
		extSize := 4

		switch h.ExtensionProfile {
		// RFC 8285 RTP One Byte Header Extension
		case extensionProfileOneByte:
			for _, extension := range h.Extensions {
				extSize += 1 + len(extension.payload)
			}
		// RFC 8285 RTP Two Byte Header Extension
		case extensionProfileTwoByte:
			for _, extension := range h.Extensions {
				extSize += 2 + len(extension.payload)
			}
		default:
			extSize += len(h.Extensions[0].payload)
		}

		// extensions size must have 4 bytes boundaries
		size += ((extSize + 3) / 4) * 4
	}

	return size
}

// SetExtension sets an RTP header extension
func (h *Header) SetExtension(id uint8, payload []byte) error {
	if h.Extension {
		switch h.ExtensionProfile {
		// RFC 8285 RTP One Byte Header Extension
		case extensionProfileOneByte:
			if id < 1 || id > 14 {
				return fmt.Errorf("header extension id must be between 1 and 14 for RFC 5285 extensions")
			}
			if len(payload) > 16 {
				return fmt.Errorf("header extension payload must be 16bytes or less for RFC 5285 one byte extensions")
			}
		// RFC 8285 RTP Two Byte Header Extension
		case extensionProfileTwoByte:
			if id < 1 || id > 255 {
				return fmt.Errorf("header extension id must be between 1 and 255 for RFC 5285 extensions")
			}
			if len(payload) > 255 {
				return fmt.Errorf("header extension payload must be 255bytes or less for RFC 5285 two byte extensions")
			}
		default: // RFC3550 Extension
			if id != 0 {
				return fmt.Errorf("header extension id must be 0 for none RFC 5285 extensions")
			}
		}

		// Update existing if it exists else add new extension
		for i, extension := range h.Extensions {
			if extension.id == id {
				h.Extensions[i].payload = payload
				return nil
			}
		}
		h.Extensions = append(h.Extensions, Extension{id: id, payload: payload})
		return nil
	}

	// No existing header extensions
	h.Extension = true

	switch len := len(payload); {
	case len <= 16:
		h.ExtensionProfile = extensionProfileOneByte
	case len > 16 && len < 256:
		h.ExtensionProfile = extensionProfileTwoByte
	}

	h.Extensions = append(h.Extensions, Extension{id: id, payload: payload})
	return nil
}

// GetExtension returns an RTP header extension
func (h *Header) GetExtension(id uint8) []byte {
	if !h.Extension {
		return nil
	}
	for _, extension := range h.Extensions {
		if extension.id == id {
			return extension.payload
		}
	}
	return nil
}

// DelExtension Removes an RTP Header extension
func (h *Header) DelExtension(id uint8) error {
	if !h.Extension {
		return fmt.Errorf("h.Extension not enabled")
	}
	for i, extension := range h.Extensions {
		if extension.id == id {
			h.Extensions = append(h.Extensions[:i], h.Extensions[i+1:]...)
			return nil
		}
	}
	return fmt.Errorf("extension not found")
}

// Marshal serializes the packet into bytes.
func (p *Packet) Marshal() (buf []byte, err error) {
	buf = make([]byte, p.MarshalSize())

	n, err := p.MarshalTo(buf)
	if err != nil {
		return nil, err
	}

	return buf[:n], nil
}

// MarshalTo serializes the packet and writes to the buffer.
func (p *Packet) MarshalTo(buf []byte) (n int, err error) {
	n, err = p.Header.MarshalTo(buf)
	if err != nil {
		return 0, err
	}

	// Make sure the buffer is large enough to hold the packet.
	if n+len(p.Payload) > len(buf) {
		return 0, io.ErrShortBuffer
	}

	m := copy(buf[n:], p.Payload)
	p.Raw = buf[:n+m]

	return n + m, nil
}

// MarshalSize returns the size of the packet once marshaled.
func (p *Packet) MarshalSize() int {
	return p.Header.MarshalSize() + len(p.Payload)
}

//miaobinwei
// Parse RFC 5285 header extension.
//func (p *Packet)ParseExtensions(){
//	externLen := len(p.ExtensionPayload)
//
//	//fmt.Println("ParseExtensions [%v][%v]",p.ExtensionProfile,p.ExtensionProfile & 65520)
//	if (p.ExtensionProfile == 0xBEDE){
//		//fmt.Println("ParseExtensions OneByte")
//		p.MapBytesExtensions = make(map[uint8]BytesExtension)
//		i:=0
//		for i <  externLen{
//			id :=  (p.ExtensionPayload[i] & 0xF0) >> 4
//			len := (p.ExtensionPayload[i] & 0x0F) + 1
//			if (id == 15){
//				break
//			}
//
//			if (id != 0){
//				if (i + 1 + int(len)) > externLen{
//					fmt.Println("not enough space for the announced One-Byte header extension element value")
//					break
//				}
//				p.MapBytesExtensions[id] = BytesExtension{id, len, p.ExtensionPayload[i + 1:i + 1 + int(len)]}
//				i = i + 1 + int(len)
//			}else{
//				i++
//			}
//			for ((i < externLen) && (0 == p.ExtensionPayload[i])){
//				i++
//			}
//		}
//	}else if ((p.ExtensionProfile & 65520) == 4096){
//		//fmt.Println("ParseExtensions TwoByte")
//		p.MapBytesExtensions = make(map[uint8]BytesExtension)
//		i := 0
//		for i + 1< externLen{
//			id := p.ExtensionPayload[i]
//			len := p.ExtensionPayload[i + 1]
//			if (id != 0){
//				if ((i + 2 + int(len)) > externLen){
//					fmt.Println("not enough space for the announced Two-Bytes header extension element value")
//					break
//				}
//				p.MapBytesExtensions[id] = BytesExtension{id, len, p.ExtensionPayload[i + 2:i + 2 + int(len)]}
//				i = i + 2 + int(len)
//			}else{
//				i++
//			}
//
//			for (i < externLen) && (0 == p.ExtensionPayload[i]){
//				i++
//			}
//		}
//	}
//
//	//for k,v := range p.MapBytesExtensions{
//	//	fmt.Printf("MapBytesExtensions k[%v] v[%v]\n",k,v)
//	//}
//}

//func (p *Packet)GetExtension(id uint8) []byte{
//	if (0 == id){
//		return nil
//	}
//
//	v,ok := p.MapBytesExtensions[id]
//	if (false == ok){
//		return nil
//	}
//
//	return v.value[:]
//
//}

func (p *Packet)IsKeyFrame()bool{
	if nil == p.PayloadDescriptorHandler{
		return false
	}

	return p.PayloadDescriptorHandler.IsKeyFrame()
}

func (p *Packet)ReadAbsSendTime(absId uint8, time *uint32) bool{
	extenValue := p.GetExtension(absId)
	if (3 != len(extenValue)){
		return false
	}
	*time = utils.Get3Bytes(extenValue,0)
	return true

}

func (p *Packet) RtxDecode(payloadType uint8, ssrc uint32) bool {
	//4) a=fmtp:97 apt=96
	//
	//   表示96类型的rtp包的重传包采用97的payloadtype的rtx包保护，rtx包的rtp header中的sequence num与rtp不一致，但timestamp一致。
	//
	//   Rtx包的payload的前两个字节为原重传rtp包的rtp sequencenum



	//所以这里替换，原来包的ssrc，seq，同时如果原始包里有pad标识，那么去掉pad的扩展字节部分，进行发送
	if (len(p.Payload) < 2) {
		return false
	}

	p.PayloadType = payloadType
	p.SequenceNumber = utils.Get2Bytes(p.Payload, 0)
	p.SSRC = ssrc
	p.Payload = p.Payload[2:]
	p.PayloadLength = p.PayloadLength - 2
	p.RawLen = p.RawLen - 2

	if (p.PayloadPadding != 0) {
		p.Padding = false

		p.RawLen -= int(p.PayloadPadding)
		p.Payload = p.Payload[:p.PayloadLength - int(p.PayloadPadding)]

		p.PayloadPadding = 0

	}

	return true

}


func (p *Packet)GetSpatialLayer() uint8{
	if (nil == p.PayloadDescriptorHandler){
		return 0
	}

	return p.PayloadDescriptorHandler.GetSpatialLayer()
}

func (p *Packet)GetTemporalLayer() uint8{
	if (nil == p.PayloadDescriptorHandler){
		return 0
	}

	return p.PayloadDescriptorHandler.GetTemporalLayer()
}


func (p *Packet)Clone() *Packet{
	packet := &Packet{}
	*packet = *p


	packet.Raw = make([]byte, len(p.Raw),  len(p.Raw))
	copy(packet.Raw, p.Raw)

	//由于发送rtp只用到 s.sendRTP(&p.Header, p.Payload)，所以暂时只拷贝这些
	packet.Payload = make([]byte, len(p.Payload),  len(p.Payload))
	copy(packet.Payload, p.Payload)


	return packet

}


