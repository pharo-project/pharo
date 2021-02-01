Class {
	#name : #RBExtractSetUpMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #'failure tests' }
RBExtractSetUpMethodTest >> testBadInterval [
	self
		shouldFail:
			(RBExtractSetUpMethodRefactoring
				extract: (14 to: 35)
				from: #testExample
				in: RBDataTest)
]

{ #category : #tests }
RBExtractSetUpMethodTest >> testExtractSetUp [
	| class refactoring |
	refactoring := RBExtractSetUpMethodRefactoring
				extract: (14 to: 29)
				from: #testExample
				in: RBDataTest.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBDataTest.

	self assert: (class parseTreeFor: #testExample) equals: (self parseMethod: 'testExample
	self assert: true').
	self assert: (class parseTreeFor: #setUp) equals: (self parseMethod: 'setUp
	super setUp.
	self someMethod')
]

{ #category : #'failure tests' }
RBExtractSetUpMethodTest >> testModelExistingSetUpMethod [
	| class refactoring |
	model := RBClassModelFactory rbNamespace new.
	class := model classNamed: #RBDataTest.
	class compile: 'setUp #setUp'
		classified: #(#accessing).
	refactoring := RBExtractSetUpMethodRefactoring 
				model: model
				extract: (14 to: 29)
				from: #testExample
				in: RBDataTest.
	self shouldFail: refactoring.
]
