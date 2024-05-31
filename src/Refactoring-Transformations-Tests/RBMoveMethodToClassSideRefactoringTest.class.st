Class {
	#name : 'RBMoveMethodToClassSideRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBMoveMethodToClassSideRefactoringTest >> testFailureExistsMethodInClassSide [

	| method someClass className refactoring |
	className := RBClassDataForRefactoringTest.
	method := className >> ('threeElement' , 'Point') asSymbol.
	refactoring := RBMoveMethodToClassSideRefactoring
		               method: method
		               class: className.
	model := refactoring model.
	someClass := model classNamed: className name , ' class'.
	someClass
		compile: 'threeElementPoint ^ self new'
		classified: { #example }.
	self
		should: [ refactoring generateChanges ]
		raise: RBRefactoringWarning
]
