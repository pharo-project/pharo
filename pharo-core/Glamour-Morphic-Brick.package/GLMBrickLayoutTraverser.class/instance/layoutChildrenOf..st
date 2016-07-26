traversing
layoutChildrenOf: aBrick

	(self prioritySortedSubbricks: aBrick) do: [ :each | self layoutAround: each inContext: aBrick ]