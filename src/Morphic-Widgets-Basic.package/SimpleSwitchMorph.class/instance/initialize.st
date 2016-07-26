initialization
initialize

	super initialize.
	self borderWidth: 3.
	self extent: self extent + 2.
	onColor := Color r: 1.0 g: 0.6 b: 0.6.
	offColor := Color lightGray.
	color := offColor
