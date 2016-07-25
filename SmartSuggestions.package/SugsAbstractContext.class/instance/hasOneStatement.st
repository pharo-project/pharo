testing
hasOneStatement
	^ (self selectedMessageName isNotNil or: [ selectedNode isLiteralNode ] ) or: [ selectedNode statements size = 1]