updating
withSiblingsDo: aBlock

	| node |
	node := self.
	[node isNil] 
		whileFalse: [
			aBlock value: node.
			node := node nextSibling].