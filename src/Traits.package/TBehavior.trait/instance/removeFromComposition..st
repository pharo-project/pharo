traits
removeFromComposition: aTrait
	self setTraitComposition: (self traitComposition copyTraitExpression
		removeFromComposition: aTrait)