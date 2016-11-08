accessing
saturation
	"Return the saturation of this color, a value between 0.0 and 1.0.
	
	Saturation is a scaling of chroma so that the whole [0..1] range is acceptable for any value of brightness/lightness.
	See #hslSaturation and #hsvSaturation; this method returns HSV saturation for compatibility."

	^ self hsvSaturation