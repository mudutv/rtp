package rtp

import (
	"github.com/mudutv/rtp/codecs"
)

const (
	MIME_TYPE_UNSET = iota
	MIME_TYPE_AUDIO
	MIME_TYPE_VIDEO
)

const (
	SUB_TYPE_UNSET = iota
	SUB_TYPE_VP8
	SUB_TYPE_VP9
	SUB_TYPE_H264

	SUB_TYPE_OPUS

	SUB_TYPE_RED
	SUB_TYPE_ULPFEC
)



type CodeType struct {
	MimeType int
	SubType  int
}

func ProcessRtpPacket(packet *Packet, codetype CodeType) {
	switch codetype.MimeType {
	case MIME_TYPE_VIDEO:
		switch codetype.SubType {
		case SUB_TYPE_H264:
			packet.PayloadDescriptorHandler = ProcessRtpPacketH264(packet)
		case SUB_TYPE_VP8:
			packet.PayloadDescriptorHandler = ProcessRtpPacketVP8(packet)
		}


	}

}


func ProcessRtpPacketVP8(packet *Packet) codecs.PayloadDescriptor{
	p := &codecs.VP8PayloadDescriptor{}
	return p.Parse(packet.Payload, len(packet.Payload),nil,0)
}

func ProcessRtpPacketH264(packet *Packet) codecs.PayloadDescriptor{
	p := &codecs.H264PayloadDescriptor{}
	return p.Parse(packet.Payload, len(packet.Payload),nil,0)
}