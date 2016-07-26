accessing
subtrees
	"Answer the subtrees of the receiver."

	^(self select: [:v | v isKindOf: KeyedTree]) values