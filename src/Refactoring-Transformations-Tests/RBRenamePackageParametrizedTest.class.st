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
	self shouldFail: (self createRefactoringWithArguments:
		{ #'Refactoring-Tests-Core' . #'Refactoring-Tests-Core' })
]

{ #category : 'failure tests' }
RBRenamePackageParametrizedTest >> testFailureExistingPackage [

	self shouldFail: (self createRefactoringWithArguments:
			 { #'Refactoring-Tests-Core'. #'Refactoring-Tests-Changes' })
]

{ #category : 'tests' }
RBRenamePackageParametrizedTest >> testRenamePackage [
	| refactoring aModel |

	refactoring := (self createRefactoringWithArguments:
		{ #'Refactoring-Tests-Core' . #'Refactoring-Tests-Core1' }).
	aModel := refactoring model.
	self executeRefactoring: refactoring.
	self assert: (aModel packageNamed: #'Refactoring-Tests-Core') isNil.
	self assert: (aModel packageNamed: #'Refactoring-Tests-Core1') isNotNil
]
