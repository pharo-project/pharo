"
I am an acceptor in the context of a RBRefactoring. I create a namespace of the current browser environment and set it as the model for the refactoring. The refactoring is executed and opens a changes browser for viewing, accepting or cancel the refactoring operation.
"
Class {
	#name : #CodeRefactoringAcceptor,
	#superclass : #AbstractAcceptor,
	#category : #'Nautilus-Acceptors'
}

{ #category : #protocol }
CodeRefactoringAcceptor >> accept: aText notifying: aController [
	| namespace refactoring changes |
	self model sourceTextModel clearUserEdits.
	namespace := RBNamespace onEnvironment: self model browsedEnvironment.
	refactoring := self class compiler evaluate: aText.
	refactoring model: namespace.
	refactoring primitiveExecute.
	changes := refactoring changes changes.
	(ChangesBrowser changes: changes) open
]
