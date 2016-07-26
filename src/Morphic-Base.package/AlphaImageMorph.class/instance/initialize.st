initialization
initialize
	"Initialize the receiver.
	Use the 32 bit depth default image to avoid
	unnecessary conversions."

	enabled := true.
	autoSize := true.
	scale := 1.0.
	layout := #topLeft.
	alpha := 1.0.
	super initialize