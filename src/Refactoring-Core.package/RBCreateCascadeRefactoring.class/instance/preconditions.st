preconditions
preconditions
	^ (RBCondition definesSelector: selector in: class) & (RBCondition withBlock: [ 
		self findSequenceNode; findStatementNodes; findReceiverNode.
		true ])