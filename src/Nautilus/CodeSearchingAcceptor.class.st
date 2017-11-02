"
I am an acceptor in the context of code searching, using RBParseTreeSearcher
"
Class {
	#name : #CodeSearchingAcceptor,
	#superclass : #AbstractAcceptor,
	#category : 'Nautilus-Acceptors'
}

{ #category : #protocol }
CodeSearchingAcceptor >> accept: aText notifying: aController [
	| environment tree rule result |
	self model sourceTextModel clearUserEdits.
	environment := self model browsedEnvironment.
	tree := self class compiler evaluate: aText.
	
	rule := CodeSearchingRule new
		matcher: tree;
		yourself.
	
	result := RBSmalllintChecker runRule: rule onEnvironment: environment.
	Smalltalk tools messageList browse: result result methods title: rule name

]
