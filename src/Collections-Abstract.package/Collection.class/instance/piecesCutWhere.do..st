enumerating
piecesCutWhere: binaryBlock do: pieceBlock
	"Evaluate pieceBlock with substrings of the receiver derived from cutting the receiver at points where binaryBlock answers true for adjacent elements."

	| size lastCut this next |
	(size := self size) <= 1 ifTrue:
		 [size = 1 ifTrue: [pieceBlock value: self].
		^self].
	lastCut := 1.
	this := self at: 1.
	2 to: size do:
		[:i|
		next := self at: i.
		(binaryBlock value: this value: next) ifTrue:
			[pieceBlock value: (self copyFrom: lastCut to: i - 1).
			lastCut := i].
		this := next].
	pieceBlock value: (self copyFrom: lastCut to: size)