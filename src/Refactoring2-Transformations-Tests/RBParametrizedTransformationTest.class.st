Class {
	#name : #RBParametrizedTransformationTest,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'model'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #parsing }
RBParametrizedTransformationTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : #parsing }
RBParametrizedTransformationTest >> parserClass [
	^ RBParser
]

{ #category : #asserting }
RBParametrizedTransformationTest >> proceedThroughWarning: aBlock [

	aBlock
		on: RBRefactoringError
		do: [ :ex | ex resume ]
]

{ #category : #'set up' }
RBParametrizedTransformationTest >> setupInlineExpressionFor: aRefactoring toReturn: aBoolean [ 
	| options |
	options := aRefactoring options copy.
	options at: #inlineExpression put: [:ref :string | aBoolean].
	aRefactoring options: options
]

{ #category : #asserting }
RBParametrizedTransformationTest >> shouldFail: aRefactoring [ 

	self proceedThroughWarning: [
		self should: [ aRefactoring transform ] raise: RBRefactoringError ]
]

{ #category : #asserting }
RBParametrizedTransformationTest >> shouldWarn: aRefactoring [ 
	self 
		should: [ aRefactoring asRefactoring transform ]
		raise: RBRefactoringWarning
]
