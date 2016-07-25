testing-basic
testCompositionFromArray
	| composition |
	composition := { (self t1) } asTraitComposition.
	self assert: (composition isKindOf: TraitComposition).
	self assert: (composition traits includes: self t1).
	self assert: composition traits size = 1.
	composition := { (self t1). self t2 } asTraitComposition.
	self assert: (composition isKindOf: TraitComposition).
	self assert: (composition traits includes: self t1).
	self assert: (composition traits includes: self t2).
	self assert: composition traits size = 2