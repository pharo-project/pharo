Class {
	#name : 'RBRenameAndDeprecateClassTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBRenameAndDeprecateClassTransformationTest >> testTransform [

	| transformation class |
	transformation := RBRenameAndDeprecateClassTransformation
		                  rename: self changeMockClass name
		                  to: #RBRefactoringChangeMock2.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 3.

	"old class"
	class := transformation model classNamed: self changeMockClass name.
	self deny: class isNil.
	self assertEmpty: class selectors.
	self assert: class superclass name equals: #RBRefactoringChangeMock2.

	"new class as a superclass"
	class := transformation model classNamed: #RBRefactoringChangeMock2.
	self denyEmpty: class selectors.

	"temporary class, should not exist"
	class := transformation model classNamed:
		         #TmpSubclass , self changeMockClass name asSymbol.
	self assert: class isNil
]
