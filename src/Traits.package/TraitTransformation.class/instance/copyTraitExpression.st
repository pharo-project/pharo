copying
copyTraitExpression
	^self shallowCopy 
		subject: self subject copyTraitExpression;
		yourself