event handling
mouseEnter: evt
	"Update the appearance."
	self setProperty: #mouseEntered toValue: true.
	
	self gradientLook
		ifTrue: [self changed]
		ifFalse: ["0.09375 is exact in floating point so no cumulative rounding error will occur"
				self color: (self color adjustBrightness: -0.09375)].
	super mouseEnter: evt