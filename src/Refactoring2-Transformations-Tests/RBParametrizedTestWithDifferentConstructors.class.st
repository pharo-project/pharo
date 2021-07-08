Class {
	#name : #RBParametrizedTestWithDifferentConstructors,
	#superclass : #RBAbstractRefactoringTest,
	#instVars : [
		'constructor'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #parameterization }
RBParametrizedTestWithDifferentConstructors >> constructor: anObject [

	constructor := anObject
]

{ #category : #builder }
RBParametrizedTestWithDifferentConstructors >> createRefactoringWithArguments: aParameterCollection [
	^ (rbClass
		perform: constructor 
		withArguments: aParameterCollection) asRefactoring
]

{ #category : #builder }
RBParametrizedTestWithDifferentConstructors >> createRefactoringWithModel: rbNamespace andArguments: aParameterCollection [
	^ (rbClass
		perform: #model: , constructor 
		withArguments: {rbNamespace}, aParameterCollection) asRefactoring
]
