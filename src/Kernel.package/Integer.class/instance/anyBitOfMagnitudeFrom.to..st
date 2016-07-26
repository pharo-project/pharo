bit manipulation
anyBitOfMagnitudeFrom: start to: stopArg 
	"Tests for any magnitude bits in the interval from start to stopArg."
	"Primitive fixed in LargeIntegers v1.2. If you have an earlier version 
	comment out the primitive call (using this ST method then)."
	| magnitude firstDigitIx lastDigitIx rightShift leftShift stop |
	<primitive: 'primAnyBitFromTo' module:'LargeIntegers'>
	start < 1 | (stopArg < 1)
		ifTrue: [^ self error: 'out of range'].
	magnitude := self abs.
	stop := stopArg min: magnitude highBit.
	start > stop
		ifTrue: [^ false].
	firstDigitIx := start - 1 // 8 + 1.
	lastDigitIx := stop - 1 // 8 + 1.
	rightShift := (start - 1 \\ 8) negated.
	leftShift := 7 - (stop - 1 \\ 8).
	firstDigitIx = lastDigitIx
		ifTrue: [| digit mask | 
			mask := (255 bitShift: rightShift negated)
						bitAnd: (255 bitShift: leftShift negated).
			digit := magnitude digitAt: firstDigitIx.
			^ (digit bitAnd: mask)
				~= 0].
	((magnitude digitAt: firstDigitIx)
			bitShift: rightShift)
			~= 0
		ifTrue: [^ true].
	firstDigitIx + 1
		to: lastDigitIx - 1
		do: [:ix | (magnitude digitAt: ix)
					~= 0
				ifTrue: [^ true]].
	(((magnitude digitAt: lastDigitIx)
			bitShift: leftShift)
			bitAnd: 255)
			~= 0
		ifTrue: [^ true].
	^ false