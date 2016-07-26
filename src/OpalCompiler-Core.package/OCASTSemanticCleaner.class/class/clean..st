api
clean: aMethodNode
	self new visitNode: aMethodNode.
	^aMethodNode