sibling
testSameNodeIsTheBrotherWhenOnlyChild
	| uniqueChildNode brother parent |
	
	parent := RBSequenceNode new.
	uniqueChildNode := RBVariableNode named: 'roque'.
	parent addNode: uniqueChildNode.
	
	self assert: uniqueChildNode parent equals: parent.
	self assert: parent children size equals: 1.
	
	brother := NNavDirectionSibling brotherNodeFor: uniqueChildNode.
	self assert: brother equals: uniqueChildNode .