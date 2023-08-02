Class {
	#name : #RBInstanceVariableStillReferenced,
	#superclass : #RBBreakingChangeReport,
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #execution }
RBInstanceVariableStillReferenced >> browse [
	"This is not satisfactory because not connected to any UI element."
	SystemNavigation default 
		browseAllAccessesTo: self refactoring variableName  
		from: self refactoring refactoredClass realClass
]

{ #category : #accessing }
RBInstanceVariableStillReferenced >> refactoring: aRefactoring [

	refactoring := aRefactoring.
	"Not sure that we need the following. "
	environment := RBBrowserEnvironment new 
		instVarRefsTo: aRefactoring variableName 
		in: aRefactoring refactoredClass realClass
]
