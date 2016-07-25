copying
copyTraitExpression
	^super copyTraitExpression 
		exclusions: self exclusions deepCopy;
		yourself