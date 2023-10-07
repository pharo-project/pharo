Class {
	#name : 'RBRenameVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#instVars : [
		'extraArgument'
	],
	#category : 'Refactoring2-Transformations-Tests-Parametrized',
	#package : 'Refactoring2-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'builder' }
RBRenameVariableParametrizedTest >> createRefactoringWithArguments: aParameterCollection [

	^ self renameVariableClass
		  perform: constructor
		  withArguments: aParameterCollection , extraArgument
]

{ #category : 'parameterization' }
RBRenameVariableParametrizedTest >> extraArgument: anObject [

	extraArgument := anObject
]

{ #category : 'parameterization' }
RBRenameVariableParametrizedTest >> renameVariableClass [
	^ rbClass
]
