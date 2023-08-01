Class {
	#name : #RBSharedVariableStillReferenced,
	#superclass : #RBBreakingChangeReport,
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #execution }
RBSharedVariableStillReferenced >> browse [

	MessageBrowserPresenter browse: (SystemNavigation default allReferencesToBinding: (self refactoring refactoredClass realClass bindingOf: self refactoring variableName))
]

{ #category : #accessing }
RBSharedVariableStillReferenced >> refactoring: aRefactoring [

	refactoring := aRefactoring.
	"Not sure that we need the following. "
	environment := RBBrowserEnvironment new 
		classVarRefsTo: aRefactoring variableName 
		in: aRefactoring refactoredClass realClass
]
