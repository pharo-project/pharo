instruction decoding
jump: dist if: bool
	| index |
	index := instructionStream pc + dist .

	dist >= 0 ifTrue: [
		"conditional jump forward"
		^ irBuilder jumpAheadTo: index if: bool ].

	self error: 'Should not do this'