private
coerce: aNumber
	"Note: this quick hack could be replaced by double dispatching"
	
	aNumber class = self class ifTrue: [^self class newFromNumber: aNumber scale: (scale max: aNumber scale)].
	(aNumber isFraction or: [aNumber isInteger]) ifTrue: [^self class newFromNumber: aNumber scale: scale].
	^aNumber