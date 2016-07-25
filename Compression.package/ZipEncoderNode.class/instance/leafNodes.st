private
leafNodes
	self isLeaf
		ifTrue:[^Array with: self]
		ifFalse:[^left leafNodes, right leafNodes]