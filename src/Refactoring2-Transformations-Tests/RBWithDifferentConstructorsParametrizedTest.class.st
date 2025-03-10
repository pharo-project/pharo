Class {
	#name : #RBWithDifferentConstructorsParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#instVars : [
		#constructor
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #parameterization }
RBWithDifferentConstructorsParametrizedTest >> constructor: anObject [

	constructor := anObject
]

{ #category : #builder }
RBWithDifferentConstructorsParametrizedTest >> createRefactoringWithArguments: aParameterCollection [
	^ (rbClass
		perform: constructor
		withArguments: aParameterCollection) asRefactoring
]

{ #category : #builder }
RBWithDifferentConstructorsParametrizedTest >> createRefactoringWithModel: rbNamespace andArguments: aParameterCollection [
	^ (rbClass
		perform: #model: , constructor
		withArguments: {rbNamespace}, aParameterCollection) asRefactoring
]
