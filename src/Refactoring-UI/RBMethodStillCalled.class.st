Class {
	#name : #RBMethodStillCalled,
	#superclass : #RBBreakingChangeReport,
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #private }
RBMethodStillCalled >> browse [
	"This is not satisfactory because not connected to any UI element."
	self halt.
	SystemNavigation default 
		browseAllAccessesTo: self refactoring variableName  
		from: self refactoring refactoredClass realClass
]
