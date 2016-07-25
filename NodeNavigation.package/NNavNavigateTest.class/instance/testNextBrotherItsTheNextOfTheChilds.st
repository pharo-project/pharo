sibling
testNextBrotherItsTheNextOfTheChilds
	| bigBrother littleBrother brother parent |
	
	parent := RBSequenceNode new.
	bigBrother := RBVariableNode named: 'pp'. 
	parent addNode: bigBrother.
	
	littleBrother := RBVariableNode named: 'roque'.
	parent addNode: littleBrother.
	
	brother := NNavDirectionSibling brotherNodeFor: bigBrother .
	self assert: brother equals: littleBrother  .
	
