testing
testTraitCompositionRespectsParenthesis
	self t1 compile: 'method ^1234'.
	self t2 compile: 'method ^4'.
	self c1 setTraitComposition: self t1 + self t2 - #method.
	self should: [ self c1 new method ] raise: MessageNotUnderstood.
	self c1 setTraitComposition: self t1 + (self t2 - #method).
	self c1 new method.
	self assert: self c1 new method = 1234