Class {
	#name : #RBRenameAndDeprecateClassTransformationTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #tests }
RBRenameAndDeprecateClassTransformationTest >> testTransform [
	| transformation class |
	transformation := (RBRenameAndDeprecateClassTransformation rename: self changeMockClass name to: #RBRefactoringChangeMock2) transform.

	self assert: transformation model changes changes size equals: 7.

	"old class"
	class := transformation model classNamed: self changeMockClass name asSymbol.
	self deny: class isNil.
	self assertEmpty: class selectors.
	self assert: class superclass name equals: #RBRefactoringChangeMock2.
	self assert: class comment equals: 'Deprecated!!! Use superclass'.

	"new class as a superclass"
	class := transformation model classNamed: #RBRefactoringChangeMock2.
	self denyEmpty: class selectors.

	"temporary class, should not exist"
	class := transformation model classNamed: #TmpSubclass , self changeMockClass name asSymbol.
	self assert: class isNil
]
