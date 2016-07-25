tests
testClassWithCachedValueByNil

	| aClassWithCachedValue result original | 
	aClassWithCachedValue := (self newSubclassOf: Object instanceVariableNames: 'cache' classVariableNames: '') 
		duringTestCompileSilently: 'cache ^cache';
		duringTestCompileSilently: 'cache: x cache := x';
		duringTestCompileSilently: '
			fuelAccept: aVisitor  
				^cache 
					ifNil: [ super fuelAccept: aVisitor ]
					ifNotNil: [ aVisitor visitSubstitution: self by: (self copy cache: nil)]';
		yourself.

	original := aClassWithCachedValue new cache: 5; yourself.

	result := self resultOfSerializeAndMaterialize: original.
	
	self assert: result cache isNil.