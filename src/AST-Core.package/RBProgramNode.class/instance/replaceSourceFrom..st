private-replacing
replaceSourceFrom: aNode
	self == aNode
		ifFalse: [ self clearReplacements ]