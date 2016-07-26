successor sequences
successorSequences

	sequence isEmpty ifTrue: [^ #()].
	^ sequence last successorSequences