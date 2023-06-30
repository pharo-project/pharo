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

{ #category : #execution }
RBInstanceVariableStillReferenced >> execute [

	SystemNavigation default 
		browseAllAccessesTo: self refactoring variableName  
		from: self refactoring refactoredClass realClass
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
