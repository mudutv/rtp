package utils


//miaobinwei
func Get2Bytes(date []byte, i int) uint16 {
	return uint16(date[i+1]) | (uint16(date[i]) << 8)
}

func Get3Bytes(data []byte, i int) uint32 {
	return (uint32(data[i+2])) | (uint32(data[i+1]) << 8) | (uint32(data[i]) << 16)
}
