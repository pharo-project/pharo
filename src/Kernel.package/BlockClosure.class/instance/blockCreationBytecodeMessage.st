scanning
blockCreationBytecodeMessage
	"Answer the abstract bytecode message that created the receiver."
	| blockCreationBytecodeSize |
	^self method abstractBytecodeMessageAt: startpc - (blockCreationBytecodeSize := 4)

	"(SortedCollection sortBlock: [:a :b| a compare: b caseSensitive: false]) sortBlock blockCreationBytecodeMessage"