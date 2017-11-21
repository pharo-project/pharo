"
A book chapter describing  important core classes from the class comments.
"
Class {
	#name : #RBCoreClassesHelp,
	#superclass : #RefactoringHelp,
	#category : #Refactoring-Help
}

{ #category : #accessing }
RBCoreClassesHelp class >> bookName [
	^ 'Core classes'
]

{ #category : #accessing }
RBCoreClassesHelp class >> pages [
	^ #(RBASTClassesHelp RBRefactoringClassesHelp RBBrowserEnvironmentsClassesHelp)
]
