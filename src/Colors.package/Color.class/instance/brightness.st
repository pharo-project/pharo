accessing
brightness
	"Return the HSV brightness of this color, a float in the range [0.0..1.0]."

	^ self privateMaxComponent asFloat / ComponentMax