private-parsing
parseWrapperPatternBlockWith: wrappedNode
	| node |

	node := self parsePatternBlock: RBPatternWrapperBlockNode.
	node class == RBPatternWrapperBlockNode ifTrue: [ 
		self flag: 'I think this is ugly, but I don''t know how to avoid checking the result of #parsePatternBlock:'.
		node wrappedNode: wrappedNode ].
	
	^ node