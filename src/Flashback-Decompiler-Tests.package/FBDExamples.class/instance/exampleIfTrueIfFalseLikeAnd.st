examples - branches
exampleIfTrueIfFalseLikeAnd
		| t1 |
	t1 := true.
	^ t1 ifTrue: [ t1 not ] ifFalse: [ false ]