Class {
	#name : 'RBRenameVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#instVars : [
		'extraArgument'
	],
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
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
