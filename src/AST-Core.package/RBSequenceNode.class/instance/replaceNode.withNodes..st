replacing
replaceNode: aNode withNodes: aCollection 
	| index newStatements |
	self clearReplacements.
	index := self indexOfNode: aNode.
	newStatements := OrderedCollection new: statements size + aCollection size.
	1 to: index - 1 do: [:i | newStatements add: (statements at: i)].
	newStatements addAll: aCollection.
	index + 1 to: statements size
		do: [:i | newStatements add: (statements at: i)].
	aCollection do: [:each | each parent: self].
	statements := newStatements