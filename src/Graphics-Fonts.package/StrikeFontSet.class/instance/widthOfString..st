measuring
widthOfString: aString

	aString ifNil:[^0].
	"Optimizing"
	(aString isByteString) ifTrue: [
		^ (self fontArray  at: 1) widthOfString: aString from: 1 to: aString size].
	^ self widthOfString: aString from: 1 to: aString size.
"
	TextStyle default defaultFont widthOfString: 'zort' 21
"
