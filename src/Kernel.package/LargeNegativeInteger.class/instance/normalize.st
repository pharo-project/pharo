converting
normalize
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen minVal |
	<primitive: 'primNormalizeNegative' module: 'LargeIntegers'>
	"First establish len = significant length"
	len := oldLen := self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len := len - 1].

	"Now check if in SmallInteger range.
	 Fast compute SmallInteger minVal digitLength"
	sLen := SmallInteger minVal < -16r40000000
				ifTrue: [8]
				ifFalse: [4].
	len <= sLen ifTrue:
		[minVal := SmallInteger minVal.
		(len < sLen
		 or: [(self digitAt: sLen) < minVal lastDigit])
			ifTrue: ["If high digit less, then can be small"
					val := 0.
					len to: 1 by: -1 do:
						[:i | val := (val *256) - (self digitAt: i)].
					^ val].
		1 to: sLen do:  "If all digits same, then = minVal"
			[:i | (self digitAt: i) = (minVal digitAt: i)
					ifFalse: ["Not so; return self shortened"
							len < oldLen
								ifTrue: [^ self growto: len]
								ifFalse: [^ self]]].
		^ minVal].

	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [^ self growto: len]
		ifFalse: [^ self]