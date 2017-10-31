collection ops
includes: aCharacter 
	| val high low |
	val := aCharacter asciiValue.
	val < 256 ifTrue: [
		^ (self byteArrayMap at: val + 1) = 1
	].
	high := val bitShift: -16.
	low := val bitAnd: 16rFFFF.
	^(self
		bitmap: (map
				at: high
				ifAbsent: [^ false])
		at: low) isZero not