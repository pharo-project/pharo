preconditions
myConditions
	| imps |
	imps := self model allImplementorsOf: oldSelector.
	self getNewSelector.
	^imps inject: (RBCondition definesSelector: oldSelector in: class)
		into: 
			[:cond :each | 
			cond 
				& (RBCondition withBlock: [(self hasReferencesToTemporaryIn: each) not]
						errorString: 'This argument is still referenced in at least one implementor!!')]