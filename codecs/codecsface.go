package codecs

type PayloadDescriptor interface {
	IsKeyFrame() bool
	GetSpatialLayer() uint8
	GetTemporalLayer() uint8
}
