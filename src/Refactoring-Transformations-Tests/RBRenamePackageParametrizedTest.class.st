Class {
	#name : 'RBRenamePackageParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBRenamePackageParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRenamePackageRefactoring };
		addCase: { #rbClass -> RBRenamePackageTransformation };
		yourself
]

{ #category : 'accessing' }
RBRenamePackageParametrizedTest >> constructor [
	^ #rename:to:
]

{ #category : 'failure tests' }
RBRenamePackageParametrizedTest >> testFailureBadName [

	self shouldFail: (self createRefactoringWithArguments: { #'Refactoring-Core-Tests'. #'Refactoring-Core-Tests' })
]

{ #category : 'failure tests' }
RBRenamePackageParametrizedTest >> testFailureExistingPackage [

	self shouldFail: (self createRefactoringWithArguments: { #'Refactoring-Core-Tests'. #'Refactoring-Changes-Tests' })
]

{ #category : 'tests' }
RBRenamePackageParametrizedTest >> testRenamePackage [

	| refactoring aModel |
	refactoring := self createRefactoringWithArguments: { #'Refactoring-Core-Tests'. #'Refactoring-Core1-Tests' }.
	aModel := refactoring model.
	self executeRefactoring: refactoring.
	self assert: (aModel packageNamed: #'Refactoring-Core-Tests') isNil.
	self assert: (aModel packageNamed: #'Refactoring-Core1-Tests') isNotNil
]
