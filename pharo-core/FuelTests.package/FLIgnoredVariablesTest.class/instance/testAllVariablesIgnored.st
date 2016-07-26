tests
testAllVariablesIgnored

	| anObject result aClass |
	aClass := self newClassWithInstanceVariableNames: 'a b'.
	aClass class duringTestCompileSilently: 'fuelIgnoredInstanceVariableNames ^#(a b)'.
	anObject := aClass new
		instVarAt: 1 put: $A;
		instVarAt: 2 put: $B;
		yourself.

	result := self resultOfSerializeAndMaterialize: anObject.

	self assert: (result instVarAt: 1) isNil.
	self assert: (result instVarAt: 2) isNil.
