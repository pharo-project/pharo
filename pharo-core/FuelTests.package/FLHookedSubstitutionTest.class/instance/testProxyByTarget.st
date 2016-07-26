tests
testProxyByTarget

	| aProxyClass result original | 
	aProxyClass := (self newSubclassOf: Object instanceVariableNames: 'target' classVariableNames: '') 
		duringTestCompileSilently: 'target: x  target := x';
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: target';
		yourself.

	original := aProxyClass new target: 5; yourself.

	result := self resultOfSerializeAndMaterialize: original.
	
	self assert: 5 = result.