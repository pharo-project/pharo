private
rootNode
	| rootNode currentNode |
	currentNode := self.
	rootNode := currentNode parentNode.
	[ rootNode isNil ]
		whileFalse: [ 
			currentNode := rootNode.
			rootNode := currentNode parentNode ].
	^ currentNode.