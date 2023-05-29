Class {
	#name : #RBComposeParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#instVars : [
		#isComposed
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #accessing }
RBComposeParametrizedTest >> createRefactoringWithArguments: aParameterCollection [
	^ isComposed
	ifTrue:
		[ {super createRefactoringWithArguments: aParameterCollection} ]
	ifFalse: [  aParameterCollection collect: [ :e |
			super createRefactoringWithArguments: {e} ]
	 ]
]

{ #category : #accessing }
RBComposeParametrizedTest >> createRefactoringWithModel: rbNamespace andArguments: aParameterCollection [
	^ isComposed
	ifTrue: [ {super createRefactoringWithModel: rbNamespace andArguments: {aParameterCollection}} ]
	ifFalse: [ aParameterCollection collect: [ :e |
		super createRefactoringWithModel: rbNamespace andArguments: {e	}
	] ]
]

{ #category : #accessing }
RBComposeParametrizedTest >> isComposed: aBoolean [
	isComposed := aBoolean
]

{ #category : #asserting }
RBComposeParametrizedTest >> shouldFail: refactoringColl [

	self proceedThroughWarning: [
		self should: [ refactoringColl do: [ :ref |
				ref primitiveExecute ] ] raise: RBRefactoringError ]
]

{ #category : #asserting }
RBComposeParametrizedTest >> shouldWarn: aRefactoringColl [
	self
		should: [ self executeRefactorings: aRefactoringColl ]
		raise: RBRefactoringWarning
]
