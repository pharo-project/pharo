code generation
sizeCode: encoder forBranchOn: condition dist: dist
	dist = 0 ifTrue: [^encoder sizePop].
	^condition
		ifTrue: [encoder sizeBranchPopTrue: dist]
		ifFalse: [encoder sizeBranchPopFalse: dist]