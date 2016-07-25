to clean later
at: key ifAbsent: aBlock
	"Answer the global associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	^globals at: key ifAbsent: aBlock