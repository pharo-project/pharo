initialize-release
noteSourceRangeStart: start end: end encoder: encoder
	"Note two source ranges for this node.  One is for the debugger
	 and is of the last expression, the result of the block.  One is for
	 source analysis and is for the entire block."
	encoder
		noteSourceRange: (start to: end)
		forNode: self closureCreationNode.
	startOfLastStatement
		ifNil:
			[encoder
				noteSourceRange: (start to: end)
				forNode: self]
		ifNotNil:
			[encoder
				noteSourceRange: (startOfLastStatement to: end - 1)
				forNode: self]