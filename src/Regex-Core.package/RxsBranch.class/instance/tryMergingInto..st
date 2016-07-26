optimization
tryMergingInto: aStream
	"Concatenation of a few simple characters can be optimized
	to be a plain substring match. Answer the node to resume
	syntax tree traversal at. Epsilon node used to terminate the branch
	will implement this to answer nil, thus indicating that the branch
	has ended."

	piece isAtomic ifFalse: [^self].
	aStream nextPut: piece character.
	^branch isNil
		ifTrue: [branch]
		ifFalse: [branch tryMergingInto: aStream]