accessing
next
	"Answer the next decompressed object in the Stream represented by the
	receiver."

	<primitive: 65>
	position >= readLimit
		ifTrue: [^self pastEndRead]
		ifFalse: [^collection at: (position := position + 1)]