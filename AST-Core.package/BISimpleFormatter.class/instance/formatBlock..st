private-formatting
formatBlock: aBlockNode
	self formatBlockArgumentsFor: aBlockNode.
	self space.
	self visitNode: aBlockNode body.
	self space

