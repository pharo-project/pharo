sibling
testNextBrotherWhenItsTheLasItsTheFirst
	| bigBrother littleBrother brother parent |
	
	parent := RBSequenceNode new.
	bigBrother := RBVariableNode named: 'pp'. 
	parent addNode: bigBrother.
	
	littleBrother := RBVariableNode named: 'roque'.
	parent addNode: littleBrother.
	
	brother := NNavDirectionSibling brotherNodeFor: littleBrother.
	self assert: brother equals: bigBrother .