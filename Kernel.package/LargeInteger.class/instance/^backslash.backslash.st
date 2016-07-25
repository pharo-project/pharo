arithmetic
\\ aNumber 
	"Primitive. Take the receiver modulo the argument. The result is the
	remainder rounded towards negative infinity, of the receiver divided
	by the argument. Fail if the argument is 0. Fail if either the argument
	or the result is not a SmallInteger or a LargePositiveInteger less than
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 31>
	aNumber isInteger
		ifTrue:
			[| neg qr q r |
			neg := self negative == aNumber negative == false.
			qr := self digitDiv: aNumber neg: neg.
			q := qr first normalize.
			r := qr last normalize.
			^(q negative
				ifTrue: [r isZero not]
				ifFalse: [q isZero and: [neg]])
					ifTrue: [r + aNumber]
					ifFalse: [r]].
	^super \\ aNumber
	