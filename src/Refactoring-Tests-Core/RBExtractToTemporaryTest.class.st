Class {
	#name : #RBExtractToTemporaryTest,
	#superclass : #RBRefactoringTest,
	#category : 'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBExtractToTemporaryTest >> testBadInterval [
	self
		shouldFail: (RBExtractToTemporaryRefactoring 
				extract: (self 
						convertInterval: (24 to: 30)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				to: 'asdf'
				from: #testMethod
				in: RBRefactoryTestDataApp);
		shouldFail: (RBExtractToTemporaryRefactoring 
				extract: (self 
						convertInterval: (14 to: 105)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod1))
				to: 'asdf'
				from: #testMethod1
				in: RBRefactoryTestDataApp);
		shouldFail: (RBExtractToTemporaryRefactoring 
				extract: (self 
						convertInterval: (61 to: 101)
						for: (RBRefactoryTestDataApp sourceCodeAt: #noMoveDefinition))
				to: 'asdf'
				from: #noMoveDefinition
				in: RBRefactoryTestDataApp)
]

{ #category : #'failure tests' }
RBExtractToTemporaryTest >> testBadName [
	self shouldFail: (RBExtractToTemporaryRefactoring 
			extract: (self 
					convertInterval: (14 to: 23)
					for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
			to: 'a sdf'
			from: #testMethod
			in: RBRefactoryTestDataApp)
]

{ #category : #tests }
RBExtractToTemporaryTest >> testExtractToTemporaryForLastStatementInBlock [
	| refactoring |
	refactoring := RBExtractToTemporaryRefactoring 
		extract: (self 
				convertInterval: (52 to: 73)
				for: (RBRefactoryTestDataApp sourceCodeAt: #caller2))
		to: 'temp'
		from: #caller2
		in: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #caller2) = (RBParser parseMethod: 'caller2
	^(1 to: 10) inject: 1 into: [:sum :each | | temp | temp := sum * (self foo: each). temp]')
]

{ #category : #tests }
RBExtractToTemporaryTest >> testExtractToTemporaryInsideBlock [
	| refactoring |
	refactoring := RBExtractToTemporaryRefactoring 
		extract: (self 
				convertInterval: (133 to: 141)
				for: (RBRefactoryTestDataApp sourceCodeAt: #noMoveDefinition))
		to: 'asdf'
		from: #noMoveDefinition
		in: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #noMoveDefinition) = (RBParser parseMethod: 'noMoveDefinition
	| temp |
	^(self collect: 
			[:each | 
			temp := each printString.
			temp , temp]) 
		select: [:each | | asdf | asdf := each size. temp := asdf + temp]')
]

{ #category : #tests }
RBExtractToTemporaryTest >> testExtractToTemporaryWithDuplicates [
	| refactoring |
	refactoring := RBExtractToTemporaryRefactoring 
		extract: (self 
				convertInterval: (73 to: 77)
				for: (RBRefactoryTestDataApp sourceCodeAt: #demoMethodWithDuplicates))
		to: 'temp'
		from: #demoMethodWithDuplicates
		in: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #demoMethodWithDuplicates) = (RBParser parseMethod: 'demoMethodWithDuplicates
	| a b result1 result2 answer temp |
	a := 3.
	temp := a + 5.
	b := temp.
	result1 := a + b.
	result2 := temp.
	answer := result1 + result2 + temp.
	^ answer')
]

{ #category : #'failure tests' }
RBExtractToTemporaryTest >> testNonExistantSelector [
	self shouldFail: (RBExtractToTemporaryRefactoring 
			extract: (14 to: 23)
			to: 'asdf'
			from: #checkClass1:
			in: RBRefactoryTestDataApp)
]
