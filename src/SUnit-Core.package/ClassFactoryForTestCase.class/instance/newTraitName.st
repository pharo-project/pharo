creating
newTraitName
	| postFix |
	postFix := (self createdTraits size + 1) printString.
	^(#TraitForTestToBeDeleted, postFix) asSymbol