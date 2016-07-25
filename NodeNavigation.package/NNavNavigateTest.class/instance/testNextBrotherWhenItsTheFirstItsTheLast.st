sibling-prev
testNextBrotherWhenItsTheFirstItsTheLast
	| bigBrother middleBrother littleBrother brother parent |
	
	parent := RBSequenceNode new.
	bigBrother := RBVariableNode named: 'big'. 
	parent addNode: bigBrother.
	
	middleBrother := RBVariableNode named: 'middle'. 
	parent addNode: middleBrother.
	
	littleBrother := RBVariableNode named: 'little'.
	parent addNode: littleBrother.
	
	brother := NNavDirectionPreviousBrother brotherNodeFor: bigBrother.
	self assert: brother equals: littleBrother .