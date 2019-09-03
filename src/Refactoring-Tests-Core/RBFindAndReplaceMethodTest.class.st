Class {
	#name : #RBFindAndReplaceMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBFindAndReplaceMethodTest >> testFindOcurrencesInOf [
	|refactoring|
	refactoring := RBFindAndReplaceMethodRefactoring find: 
		(RBRefactoryTestDataApp >> #textInput:name:symbol:).
	self assert: (refactoring findOcurrencesIn: #renderContentOn: of: RBRefactoryTestDataApp ) size
		equals: 4
]

{ #category : #tests }
RBFindAndReplaceMethodTest >> testReplaceArgumentsByPattern [
	|code patternCode|
	code := 'foo: argMatch0 bar: argMatch1 
	|tempMatch0| 
	tempMatch0 := 4.
	^ argMatch0, argMatch1, tempMatch0 asString'.
	patternCode := 'foo: `@argMatch0 bar: `@argMatch1 
	|`@tempMatch0| 
	`@tempMatch0 := 4.
	^ `@argMatch0, `@argMatch1, `@tempMatch0 asString'.
	
	self assert: (RBFindAndReplaceMethodRefactoring new replaceArgumentsByPattern: code)
		equals: patternCode 
]
