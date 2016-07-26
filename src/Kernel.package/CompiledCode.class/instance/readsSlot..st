scanning
readsSlot: aSlot
	| nodes |
	nodes := self ast allChildren.
	nodes := nodes select: #isVariable.
	nodes := nodes select: #isInstance.
	nodes := nodes reject: [ :node | node parent isAssignment and: [ node parent variable = node ] ].
	^ nodes anySatisfy: [ :node | node binding slot == aSlot ]