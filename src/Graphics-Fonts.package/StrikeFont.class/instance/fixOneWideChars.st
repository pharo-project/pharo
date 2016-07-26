character shapes
fixOneWideChars
	"This fixes all 1-wide characters to be 2 wide with blank on the right
	so as not to cause artifacts in neighboring characters in bold or italic."
	| twoWide |
	minAscii 
		to: maxAscii
		do: 
			[ :i | 
			(self widthOf: (Character value: i)) = 1 ifTrue: 
				[ twoWide := Form extent: 2 @ glyphs height.
				(self characterFormAt: (Character value: i)) 
					displayOn: twoWide
					at: 0 @ 0.
				self 
					characterFormAt: (Character value: i)
					put: twoWide ] ]
	"
StrikeFont allInstancesDo: [:f | f fixOneWideChars].
StrikeFont shutDown.  'Flush synthetic fonts'.
"