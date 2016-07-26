private-bit-manipulation
reflect: anInteger onLowerBits: anotherInteger
	| register test |
	register := anInteger.
	test := anInteger.
	0 to: anotherInteger - 1 do: [ :index |
		register := 
			(test bitAnd: 1) = 1
				ifTrue: [ register bitOr: (self bitMaskAt: anotherInteger - index) ]
				ifFalse: [ register bitAnd: (self invertedBitMaskAt: anotherInteger - index) ].
	test := test bitShift: -1 ].

	^ register