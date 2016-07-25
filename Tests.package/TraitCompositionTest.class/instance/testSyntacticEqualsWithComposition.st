testing-comparing
testSyntacticEqualsWithComposition

	self assert: ({} asTraitComposition syntacticallyEquals: {} asTraitComposition).
	self deny: ({} asTraitComposition syntacticallyEquals: self t1 asTraitComposition).

	self assert: (self t1 asTraitComposition syntacticallyEquals: self t1 asTraitComposition).
	self deny: (self t1 asTraitComposition syntacticallyEquals: self t2 asTraitComposition).
	
	self assert: ((self t1 + self t2) syntacticallyEquals: (self t1 + self t2)).
	self deny: ((self t1 + self t2) syntacticallyEquals: (self t1 + self t3)).
	self deny: ((self t1 + self t2) syntacticallyEquals: (self t2 + self t1)).
	
	self assert: ((self t1 + self t2 + self t3) syntacticallyEquals: (self t1 + self t2 + self t3)).
	self deny: ((self t1 + self t2 + self t3) syntacticallyEquals: (self t3 + self t2 + self t1)).
