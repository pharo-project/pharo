accessing
traitComposition: aTraitComposition
	" We copy the trait composition since we might want "
	" to update the slot indices if the composition is partly stateful "
	traitComposition := aTraitComposition copyTraitExpression