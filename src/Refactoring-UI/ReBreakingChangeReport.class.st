Class {
	#name : 'ReBreakingChangeReport',
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
ReBreakingChangeReport >> environment [

	^ environment
]

{ #category : 'accessing' }
ReBreakingChangeReport >> refactoring [
	^ refactoring
]

{ #category : 'accessing' }
ReBreakingChangeReport >> refactoring: aRefactoring [

	refactoring := aRefactoring.
	environment := RBBrowserEnvironment new 
		instVarRefsTo: aRefactoring variableName 
		in: aRefactoring refactoredClass realClass
]
