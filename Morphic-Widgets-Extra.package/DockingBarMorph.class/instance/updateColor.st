private - layout
updateColor
	"private - update the receiver's color"
	| fill |
	self autoGradient
		ifFalse: [^ self].
	""
	fill := GradientFillStyle ramp: self gradientRamp.
	""
	fill origin: self topLeft.
	self isVertical
		ifTrue: [fill direction: self width @ 0]
		ifFalse: [fill direction: 0 @ self height].
	""
	self fillStyle: fill