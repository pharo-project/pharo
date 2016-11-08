accessing
color: aColor
	"Check to avoid repeats of the same color."

	aColor ifNil: [^self].
	(lastColor = aColor and: [
	self getModelState = (self lastState)])
		ifTrue: [^self].
	super color: aColor.
	self class gradientButtonLook ifTrue: [self adoptColor: aColor]