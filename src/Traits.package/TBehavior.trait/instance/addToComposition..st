traits
addToComposition: aTrait
	self setTraitComposition: (self traitComposition copyTraitExpression
		add: aTrait;
		yourself)