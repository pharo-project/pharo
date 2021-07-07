Class {
	#name : #RBVariableParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#instVars : [
		'constructor'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #parameterization }
RBVariableParametrizedTest >> constructor: anObject [

	constructor := anObject
]

{ #category : #builder }
RBVariableParametrizedTest >> createRefactoringWithArguments: aParameterCollection [
	^ (rbClass
		perform: constructor 
		withArguments: aParameterCollection) asRefactoring
]

{ #category : #builder }
RBVariableParametrizedTest >> createRefactoringWithModel: rbNamespace andArguments: aParameterCollection [
	^ (rbClass
		perform: #model: , constructor 
		withArguments: {rbNamespace}, aParameterCollection) asRefactoring
]
