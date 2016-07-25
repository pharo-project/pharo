initialization
initialize
	"Initialize the receiver from the current theme."
	
	super initialize.
	value := 0.
	start := 0.
	end := 100.
	height := DefaultHeight.
	width := DefaultWidth.
	cachedWidth := 0.
	self 
		fillStyle: (self theme progressBarFillStyleFor: self);
		borderStyle: (self theme progressBarBorderStyleFor: self);
		barFillStyle: (self theme progressBarProgressFillStyleFor: self);
		extent: width@height  + (2 * self borderWidth).