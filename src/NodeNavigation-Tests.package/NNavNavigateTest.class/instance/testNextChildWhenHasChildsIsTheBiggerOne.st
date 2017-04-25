children
testNextChildWhenHasChildsIsTheBiggerOne
	| bigBrother littleBrother child parentRenamed |
	
	parentRenamed := RBSequenceNode new.
	bigBrother := RBVariableNode named: 'pp'. 
	parentRenamed addNode: bigBrother.
	
	littleBrother := RBVariableNode named: 'roque'.
	parentRenamed addNode: littleBrother.
	
	child := NNavDirectionFirstChild childNodeFor:  parentRenamed .
	self assert: child equals: bigBrother .