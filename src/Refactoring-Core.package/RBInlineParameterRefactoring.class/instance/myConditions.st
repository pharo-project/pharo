preconditions
myConditions
	self getNewSelector.
	expressions := self allExpressionsToInline.
	^(RBCondition definesSelector: oldSelector in: class) 
		& ((RBCondition withBlock: [expressions isEmpty not]) 
				errorMacro: 'No callers. Use Remove Method instead.') 
			& ((RBCondition withBlock: [expressions size = 1]) 
					errorMacro: 'All values passed as this argument must be identical.') 
			& ((RBCondition withBlock: [expressions first isLiteralNode]) 
					errorMacro: 'All values passed must be literal.')