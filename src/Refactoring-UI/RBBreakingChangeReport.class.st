Class {
	#name : 'RBBreakingChangeReport',
	#superclass : 'Object',
	#instVars : [
		'environment',
		'refactoring'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
RBBreakingChangeReport >> environment [

	^ environment
]

{ #category : 'accessing' }
RBBreakingChangeReport >> refactoring [
	^ refactoring
]

{ #category : 'accessing' }
RBBreakingChangeReport >> refactoring: aRefactoring [

	refactoring := aRefactoring.
	environment := RBBrowserEnvironment new 
		instVarRefsTo: aRefactoring variableName 
		in: aRefactoring refactoredClass realClass
]
