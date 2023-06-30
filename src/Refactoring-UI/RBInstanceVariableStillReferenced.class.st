Class {
	#name : #RBInstanceVariableStillReferenced,
	#superclass : #RBBreakingChangeReport,
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #execution }
RBInstanceVariableStillReferenced >> execute [

	SystemNavigation default 
		browseAllAccessesTo: self refactoring variableName  
		from: self refactoring refactoredClass realClass
]
