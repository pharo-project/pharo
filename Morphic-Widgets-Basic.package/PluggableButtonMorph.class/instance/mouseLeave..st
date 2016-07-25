event handling
mouseLeave: evt
	"Update the appearance."
	self setProperty: #mouseEntered toValue: false.
	self gradientLook
		ifTrue: [self changed]
		ifFalse: ["0.09375 is exact in floating point so no cumulative rounding error will occur"
				self color: (self color adjustBrightness: 0.09375).
				self update: nil].
	super mouseLeave: evt