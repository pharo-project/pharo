Class {
	#name : #RBSharedVariableStillReferenced,
	#superclass : #RBBreakingChangeReport,
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #execution }
RBSharedVariableStillReferenced >> browse [
	"This is not satisfactory because not connected to any UI element."
	SystemNavigation default 
		browseAllAccessesTo: self refactoring variableName  
		from: self refactoring refactoredClass realClass
]

{ #category : #accessing }
RBSharedVariableStillReferenced >> refactoring: aRefactoring [

	refactoring := aRefactoring.
	"Not sure that we need the following. "
	environment := RBBrowserEnvironment new 
		classVarRefsTo: aRefactoring variableName 
		in: aRefactoring refactoredClass realClass
]
