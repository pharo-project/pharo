Class {
	#name : 'RBWithDifferentConstructorsParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#instVars : [
		'constructor'
	],
	#category : 'Refactoring2-Transformations-Tests-Parametrized',
	#package : 'Refactoring2-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'parameterization' }
RBWithDifferentConstructorsParametrizedTest >> constructor: anObject [

	constructor := anObject
]

{ #category : 'builder' }
RBWithDifferentConstructorsParametrizedTest >> createRefactoringWithArguments: aParameterCollection [

	^ rbClass perform: constructor withArguments: aParameterCollection
]

{ #category : 'builder' }
RBWithDifferentConstructorsParametrizedTest >> createRefactoringWithModel: rbNamespace andArguments: aParameterCollection [

	^ rbClass
		  perform: #model: , constructor
		  withArguments: { rbNamespace } , aParameterCollection
]
