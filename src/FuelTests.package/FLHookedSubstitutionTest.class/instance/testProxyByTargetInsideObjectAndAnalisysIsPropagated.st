tests
testProxyByTargetInsideObjectAndAnalisysIsPropagated

	| aProxyClass result original pair pairRoot | 
	aProxyClass := (self newSubclassOf: Object instanceVariableNames: 'target' classVariableNames: '') 
		duringTestCompileSilently: 'target: x  target := x';
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: target';
		yourself.

	pair := FLPair new.
	pair left: 6.
	pair right: 'foo'. 

	original := aProxyClass new target: pair; yourself.

	pairRoot := FLPair new left: original; yourself.

	result := self resultOfSerializeAndMaterialize: pairRoot.
	
	self assert: result left left = 6.
	self assert: result left right = 'foo'.
	self assert: result right isNil.
			
