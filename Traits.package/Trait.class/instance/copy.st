copying
copy 
	| newTrait |
	newTrait := self class basicNew initialize
		name: self name
		traitComposition: self traitComposition copyTraitExpression 
		methodDict: self methodDict copy
		localSelectors: self localSelectors copy
		organization: self organization copy.
	
	newTrait environment: self environment.	
	newTrait classTrait initializeFrom: self classTrait.
	^newTrait