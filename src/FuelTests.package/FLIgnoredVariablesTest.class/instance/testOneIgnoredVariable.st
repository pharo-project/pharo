tests
testOneIgnoredVariable

	| anObject result aClass |
	aClass := self newClassWithInstanceVariableNames: 'a b c'.
	aClass class duringTestCompileSilently: 'fuelIgnoredInstanceVariableNames ^#(b)'.
	anObject := aClass new
		instVarAt: 1 put: $A;
		instVarAt: 2 put: $B;
		instVarAt: 3 put: $C;
		yourself.

	result := self resultOfSerializeAndMaterialize: anObject.

	self assert: $A = (result instVarAt: 1).
	self assert: nil = (result instVarAt: 2).
	self assert: $C = (result instVarAt: 3)