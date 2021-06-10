Class {
	#name : #RBRemoveClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #accessing }
RBRemoveClassTransformationTest class >> defaultTimeLimit [
	^20 seconds
]

{ #category : #private }
RBRemoveClassTransformationTest >> resumeIfCannotRemove: error [

	^ error messageText = RBRemoveClassTransformation
		cannotRemoveTopLevelClassErrorMesssage
		ifTrue: [ error resume ]
]

{ #category : #tests }
RBRemoveClassTransformationTest >> testRefactoring [
	| refactoring |
	refactoring := (RBRemoveClassTransformation className: #RBFooDummyLintRuleTest1) asRefactoring.

	[ refactoring transform ]
		on: RBRefactoringError
		do: [ :error | self resumeIfCannotRemove: error ].

	self assert: (refactoring model classNamed: #RBFooDummyLintRuleTest1) isNil.
	self assert: (refactoring model classNamed: 'RBTransformationDummyRuleTest1' asSymbol) superclass equals: (refactoring model classNamed: #RBDummyLintRuleTest)
]

{ #category : #tests }
RBRemoveClassTransformationTest >> testRemoveClassWithBadName [

	self shouldFail: (RBRemoveClassTransformation
							className: #RecursiveSelfRule)
							asRefactoring 
]

{ #category : #tests }
RBRemoveClassTransformationTest >> testRemoveClassWithReferences [

	self shouldFail: (RBRemoveClassTransformation
							className: #RBBasicLintRuleTestData)
							asRefactoring 
]

{ #category : #tests }
RBRemoveClassTransformationTest >> testRemoveClassWithSubclasses [

	self shouldFail: (RBRemoveClassTransformation
							className: #RBFooLintRuleTestData1)
							asRefactoring
]

{ #category : #tests }
RBRemoveClassTransformationTest >> testTransform [

	| transformation newClass superclass |
	transformation := (RBRemoveClassTransformation
							className: self changeMock name)
							transform.
	self assert: transformation model changes changes size equals: 1.
	newClass := transformation model classNamed: self changeMock name asSymbol.
	superclass := transformation model classNamed: #Object.
	self assert: newClass isNil.
	newClass := self changeMock name.
	self assert: ((superclass subclasses collect: [:each | each name]) includes: newClass) not.
	self assert: ((superclass theMetaClass subclasses collect: [:each | each name]) includes: newClass) not.
]
