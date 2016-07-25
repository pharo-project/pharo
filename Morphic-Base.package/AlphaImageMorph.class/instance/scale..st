accessing
scale: aNumber
	"Set the value of scale"

	scale = aNumber ifTrue: [^self].
	scale := aNumber.
	self
		cachedForm: nil;
		changed;
		changed: #scale