testing-basic
testSum
	| composition |
	composition := self t1 + self t2 + self t3.
	self assert: (composition isKindOf: TraitComposition).
	self assert: (composition traits includes: self t1).
	self assert: (composition traits includes: self t2).
	self assert: (composition traits includes: self t3).
	self assert: composition traits size = 3