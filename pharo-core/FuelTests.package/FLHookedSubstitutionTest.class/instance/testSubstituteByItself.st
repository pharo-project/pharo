tests
testSubstituteByItself

	| aRecursiveClass result | 
	aRecursiveClass := (self newSubclassOf: Object instanceVariableNames: '' classVariableNames: '') 
		duringTestCompileSilently: 'fuelAccept: aMapper  
			^aMapper 
				visitSubstitution: self by: self 
				onRecursionDo: [super fuelAccept: aMapper]';
		yourself.

	result := self resultOfSerializeAndMaterialize: aRecursiveClass new.	
	
	"self flag: #todo." "Should test that it doesn't enter in loop."