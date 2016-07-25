other events
mouseDownInSlider: event
	"Ignore if disabled."
	
	self enabled ifFalse: [^self].
	^super mouseDownInSlider: event