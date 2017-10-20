tests - iterate
testAnySatisfy

	self assert: ( self collection anySatisfy: [:each | each = -2]).
	self deny: (self collection anySatisfy: [:each | each isString]).