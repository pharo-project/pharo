sibling-prev
testNotPreviousBrotherWhenOrphan
	| orphanNode brother |
	
	orphanNode := RBVariableNode named: 'roque'.
	brother := NNavDirectionPreviousBrother  brotherNodeFor: orphanNode.
	self assert: brother isNil.