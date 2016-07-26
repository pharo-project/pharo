other events
mouseDownInSlider: event
	"The mouse has been pressed in the slider area."
	
	interval = 1.0 ifTrue:
		["make the entire scrollable area visible if a full scrollbar is clicked on"
		self setValue: 0.
		self model hideOrShowScrollBar].
	event redButtonPressed ifFalse: [^self].
	slider fillStyle: self pressedThumbFillStyle.
	slider borderStyle: self pressedThumbBorderStyle.
	self theme useScrollbarThumbShadow ifTrue: [
		sliderShadow
			color: self sliderShadowColor;
			cornerStyle: slider cornerStyle;
			bounds: slider bounds;
			show]