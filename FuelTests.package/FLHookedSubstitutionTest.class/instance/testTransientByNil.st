tests
testTransientByNil

	| result aClassOfTransientObjects | 
	aClassOfTransientObjects := self newClass
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: nil';
		yourself.

	result := self resultOfSerializeAndMaterialize: aClassOfTransientObjects new.
	
	self assert: result isNil.