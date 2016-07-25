accessing
hue: aFloat
	"Set the hue in the range 0.0 - 1.0. Update the SV morph and hMorph."

	self hMorph value: aFloat.
	self svMorph color: (Color h: aFloat * 359.9 s: 1.0 v: 1.0)