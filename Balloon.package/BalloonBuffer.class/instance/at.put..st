accessing
at: index put: anInteger
	"For simulation only"
	| word |
	anInteger < 0
		ifTrue:["word := 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self  basicAt: index put: word.
	^anInteger