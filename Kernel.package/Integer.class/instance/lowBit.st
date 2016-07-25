bit manipulation
lowBit
	"Answer the index of the low order bit of this number."
	| index |
	self = 0 ifTrue: [ ^ 0 ].
	index := 1.
	[ (self digitAt: index) = 0 ]
		whileTrue:
			[ index := index + 1 ].
	^ (self digitAt: index) lowBit + (8 * (index - 1))