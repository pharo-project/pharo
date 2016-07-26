testing-comparing
testSyntacticEqualsWithExclusion

	self assert: ((self t1 - {#a}) syntacticallyEquals: (self t1 - {#a})).
	self deny: ((self t1 - {#a}) syntacticallyEquals: (self t1 - {#b})).
	self deny: ((self t1 - {#a. #b}) syntacticallyEquals: (self t1 - {#b. #a})).
	
	self assert: ((self t1 - {#a} - {#b}) syntacticallyEquals: (self t1 - {#a} - {#b})).
	self deny: ((self t1 - {#a} - {#b}) syntacticallyEquals: (self t1 - {#b} - {#a})).
