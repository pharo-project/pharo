tests
testProxyInsideObjectByTarget

	| aProxyClass result original pair | 
	aProxyClass := (self newSubclassOf: Object instanceVariableNames: 'target' classVariableNames: '') 
		duringTestCompileSilently: 'target: x  target := x';
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: target';
		yourself.

	original := aProxyClass new target: 5; yourself.
	
	pair := FLPair new.
	pair left: original.
	pair right: 'foo'. 

	result := self resultOfSerializeAndMaterialize: pair.
	
	self assert: result left = 5.
	self assert: result right = 'foo'.
		
