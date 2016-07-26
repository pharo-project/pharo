parsing-private
readScaleWithDefaultNumberOfDigits: anInteger
	"Read the scale if any and store it into scale instance Variable.
	Answer true if found, answer false if none.
	The scale is specified by letter s, optionnally followed by a positive integer in base 10.
	If no integer is specified, that means using as many digits as provided after the fraction separator, as provided by parameter anInteger.
	A letter s followed by another letter is not considered as a scale specification, because it could be part of a message."

	scale := 0.
	sourceStream atEnd
		ifTrue: [ ^ false ].
	(sourceStream peekFor: $s)
		ifFalse: [ ^ false ].
	scale := self nextUnsignedIntegerOrNilBase: 10.
	scale
		ifNil: [ 
			scale := anInteger.
			(sourceStream peek ifNil: [ false ] ifNotNil: [ :nextChar | nextChar isLetter ])
				ifTrue: [ 
					sourceStream skip: -1.	"ungobble the s"
					^ false ]
				ifFalse: [ ^ true ] ].
	^ true