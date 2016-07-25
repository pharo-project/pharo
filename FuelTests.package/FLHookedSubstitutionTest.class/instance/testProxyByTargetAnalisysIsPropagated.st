tests
testProxyByTargetAnalisysIsPropagated

	| aProxyClass result pair | 
	self flag: #todo. "I don't understand what are we testing here."

	aProxyClass := (self newSubclassOf: Object instanceVariableNames: 'target' classVariableNames: '') 
		duringTestCompileSilently: 'target: x  target := x';
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: target';
		yourself.

	pair := FLPair new.
	pair left: 6.
	pair right: 'foo'. 

	aProxyClass new target: pair; yourself.

	result := self resultOfSerializeAndMaterialize: pair.
	
	self assert: result left = 6.
	self assert: result right = 'foo'.