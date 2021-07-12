Class {
	#name : #RBRenamePackageParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRenamePackageParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRenamePackageRefactoring };
		yourself
]

{ #category : #'failure tests' }
RBRenamePackageParametrizedTest >> testBadName [
	self
		shouldFail: (rbClass 
				rename: #'Refactoring-Tests-Core'
				to: #'Refactoring-Tests-Core')
]

{ #category : #'failure tests' }
RBRenamePackageParametrizedTest >> testExistingPackage [
	self
		shouldFail: (rbClass 
				rename: #'Refactoring-Tests-Core'
				to: #'Refactoring-Tests-Changes')
]

{ #category : #tests }
RBRenamePackageParametrizedTest >> testRenamePackage [
	| refactoring aModel |
	
	refactoring := (rbClass 
				rename: #'Refactoring-Tests-Core'
				to: #'Refactoring-Tests-Core1').
	aModel := refactoring model.
	self executeRefactoring: refactoring.
	self assert: (aModel packageNamed: #'Refactoring-Tests-Core') isNil.
	self assert: (aModel packageNamed: #'Refactoring-Tests-Core1') isNotNil.
	
]
