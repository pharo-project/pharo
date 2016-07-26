accessing
sortBlock
	"Answer the block to sort tree keys with."

	^[:a :b | [a <= b] on: Error do: [a class name <= b class name]]