Class {
	#name : #RBInstanceVariableStillReferenced,
	#superclass : #RBBreakingChangeReport,
	#instVars : [
		'environment',
		'refactoring'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #accessing }
RBInstanceVariableStillReferenced >> environment [

	^ environment
]

{ #category : #accessing }
RBInstanceVariableStillReferenced >> refactoring [
	^ refactoring
]

{ #category : #accessing }
RBInstanceVariableStillReferenced >> refactoring: aRefactoring [

	refactoring := aRefactoring.
	environment := RBBrowserEnvironment new 
		instVarRefsTo: aRefactoring variableName 
		in: aRefactoring refactoredClass realClass
]
