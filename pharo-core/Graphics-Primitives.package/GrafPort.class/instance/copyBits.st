copying
copyBits
	"Override copybits to do translucency if desired"

	(combinationRule between: 30 and: 31)
		ifTrue: [ self copyBitsTranslucent: (alpha ifNil: [ 255 ]) ]
		ifFalse: [ super copyBits ]