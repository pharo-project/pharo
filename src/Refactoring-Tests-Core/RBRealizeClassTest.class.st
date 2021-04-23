Class {
	#name : #RBRealizeClassTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #tests }
RBRealizeClassTest >> createAbstractClass [ 
	(model classNamed: #RBFooLintRuleTestData) 
		compile: 'bar ^ self subclassResponsibility'
		classified: #(#accessing).
	(model classNamed: #RBFooLintRuleTestData) classSide
		compile: 'foo ^ self subclassResponsibility'
		classified: #(#accessing).
	
	
]

{ #category : #tests }
RBRealizeClassTest >> testClassWithoutChanges [
	self executeRefactoring: (RBRealizeClassRefactoring 
		model: model 
		className: #RBLintRuleTestData).
	self assert: model changes changes isEmpty
]

{ #category : #tests }
RBRealizeClassTest >> testRealizeAbstractClass [
	| class |
	class := model classNamed: #RBFooLintRuleTestData.
	self createAbstractClass.
	self proceedThroughWarning: [ self executeRefactoring: 
		(RBRealizeClassRefactoring 
		model: model 
		className: #RBFooLintRuleTestData)].
	self assert: (class parseTreeFor: #isEmpty) equals: 
	(self parseMethod: 'isEmpty
		self shouldBeImplemented').
	self assert: (class parseTreeFor: #problemCount) equals: 
	(self parseMethod: 'problemCount
		self shouldBeImplemented').
	self assert: (class parseTreeFor: #viewResults) equals: 
	(self parseMethod: 'viewResults
		self shouldBeImplemented') 
]
