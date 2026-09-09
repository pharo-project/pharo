Class {
	#name : 'RBMoveMethodToClassSideRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBMoveMethodToClassSideRefactoringTest >> testFailureExistsMethodInClassSide [

	| method aMetaClass refactoring |
	method := RBClassDataForRefactoringTest >> ('threeElement' , 'Point') asSymbol.
	refactoring := ReMoveMethodToClassSideRefactoring
		               method: method
		               class: RBClassDataForRefactoringTest.
	model := refactoring model.
	
	aMetaClass := model classNamed: RBClassDataForRefactoringTest name , ' class'.
	
	aMetaClass
		compile: 'threeElementPoint ^ self new'
		classified: { #example }.

	self 
		should: [ refactoring checkBreakingChangePreconditions ]
		raise: RBRefactoringWarning
]

{ #category : 'failure tests' }
RBMoveMethodToClassSideRefactoringTest >> testMoveMethodToClassSideIsOk [

	| method refactoring changes |
	method := RBClassDataForRefactoringTest >> ('threeElement' , 'Point') asSymbol.
	refactoring := ReMoveMethodToClassSideRefactoring
		               method: method
		               class: RBClassDataForRefactoringTest.
	changes := refactoring generateChanges changes.
	self assert: changes size equals: 3
	

]
