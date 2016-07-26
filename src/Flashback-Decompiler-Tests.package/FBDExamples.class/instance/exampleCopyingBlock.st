examples - blocks
exampleCopyingBlock
	| t |
	t := true.
	^ [ t ifTrue: [ 5 + t asBit ]. t ].