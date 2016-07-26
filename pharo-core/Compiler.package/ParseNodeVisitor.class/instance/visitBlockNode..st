visiting
visitBlockNode: aBlockNode
	aBlockNode statements do:
		[:statement| statement accept: self]