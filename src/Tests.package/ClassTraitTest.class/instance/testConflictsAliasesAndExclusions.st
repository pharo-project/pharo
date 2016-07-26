testing
testConflictsAliasesAndExclusions
	"conflict"

	self t1 classTrait compile: 'm2ClassSide: x ^99' classified: 'mycategory'.
	self assert: (self t1 classTrait includesLocalSelector: #m2ClassSide:).
	self assert: (self t5 classTrait >> #m2ClassSide:) isConflict.
	self assert: (self c2 class >> #m2ClassSide:) isConflict.

	"exclusion and alias"
	self assert: self t5 classSide traitComposition asString 
				= 'T1 classTrait + T2 classTrait'.
	self t5 classSide 
		setTraitCompositionFrom: (self t1 classTrait @ { (#m2ClassSideAlias1: -> #m2ClassSide:) } 
				+ ((self t2 classTrait) @ { (#m2ClassSideAlias2: -> #m2ClassSide:) } 
				- { #m2ClassSide: })).
	self deny: (self t5 classTrait >> #m2ClassSide:) isConflict.
	self deny: (self c2 class >> #m2ClassSide:) isConflict.
	self assert: (self c2 m2ClassSideAlias1: 13) = 99.
	self assert: (self c2 m2ClassSideAlias2: 13) = 13