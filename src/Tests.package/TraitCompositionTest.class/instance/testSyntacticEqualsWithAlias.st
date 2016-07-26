testing-comparing
testSyntacticEqualsWithAlias

	self assert: ((self t1 @ {#a -> #b}) syntacticallyEquals: (self t1 @ {#a -> #b})).
	self deny: ((self t1 @ {#a -> #b}) syntacticallyEquals: self t1).
	self deny: ((self t1 @ {#a -> #b}) syntacticallyEquals: (self t1 @ {#a -> #B})).
	self deny: ((self t1 @ {#a -> #b}) syntacticallyEquals: (self t1 @ {#A -> #b})).

	self assert: ((self t1 @ {#a -> #b} @ {#x -> #y}) syntacticallyEquals: (self t1 @ {#a -> #b} @ {#x -> #y})).
	self deny: ((self t1 @ {#a -> #b} @ {#x -> #y}) syntacticallyEquals: (self t1 @ {#x -> #y} @ {#a -> #b})).