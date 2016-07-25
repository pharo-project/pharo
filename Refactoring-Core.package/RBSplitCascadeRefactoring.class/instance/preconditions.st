preconditions
preconditions
	^ (RBCondition definesSelector: selector in: class) & (RBCondition withBlock: [ 
		self findCascadeNode; findAncestorNode; findMessageNodes. 
		true ])