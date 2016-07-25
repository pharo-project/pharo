code generation
emitCodeForBranchOn: condition dist: dist pop: stack encoder: encoder
	stack pop: 1.
	dist = 0 ifTrue: [^encoder genPop].
	condition
		ifTrue: [encoder genBranchPopTrue: dist]
		ifFalse: [encoder genBranchPopFalse: dist]