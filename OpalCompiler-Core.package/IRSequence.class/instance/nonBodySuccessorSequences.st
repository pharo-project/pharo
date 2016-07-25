successor sequences
nonBodySuccessorSequences

	sequence isEmpty ifTrue: [^ #()].
	^ sequence last nonBodySuccessorSequences