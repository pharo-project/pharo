linked frames
isLinkedTo: aMorph
	self firstInChain withSuccessorsDo:
		[:m | m == aMorph ifTrue: [^ true]].
	^ false