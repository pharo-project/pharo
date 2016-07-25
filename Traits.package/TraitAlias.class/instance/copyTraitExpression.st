copying
copyTraitExpression
	^super copyTraitExpression
		aliases: self aliases deepCopy;
		yourself