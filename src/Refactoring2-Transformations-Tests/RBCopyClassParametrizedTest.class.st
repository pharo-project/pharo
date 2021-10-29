Class {
	#name : #RBCopyClassParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBCopyClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBCopyClassRefactoring };
		addCase: { #rbClass -> RBCopyClassTransformation };
		yourself
]

{ #category : #accessing }
RBCopyClassParametrizedTest >> constructor [ 
	^ #copyClass:withName:
]

{ #category : #accessing }
RBCopyClassParametrizedTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #accessing }
RBCopyClassParametrizedTest >> testCopyClass [
	| class refactoring copiedClass|
	refactoring := self createRefactoringWithModel: model
		andArguments: { #RBDummyEmptyClass . #RBDummyEmptyClassCopy }.
	class := refactoring model classNamed: #RBDummyEmptyClass.
	self executeRefactoring: refactoring .
	copiedClass :=  refactoring model classNamed: #RBDummyEmptyClassCopy.
	self assert: class superclass equals: copiedClass superclass.
	self assert: (class directlyDefinesMethod: #someMethod).
	self assert: (copiedClass directlyDefinesMethod: #someMethod)
	

]
