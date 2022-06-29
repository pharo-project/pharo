"
I am simple perform method remove operation without any extra logic
"
Class {
	#name : #SycSilentlyRemoveMethodStrategy,
	#superclass : #SycRemoveMethodStrategy,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #removing }
SycSilentlyRemoveMethodStrategy >> removeMethods: methods [
	
	| refactorings |
	refactorings := OrderedCollection new.
	refactorings := methods collect: [ :each | 
		RBRemoveMethodTransformation 
			selector: each selector 
			from: each methodClass ].
	refactorings do: [ :each | each asRefactoring execute ]
]

{ #category : #removing }
SycSilentlyRemoveMethodStrategy >> userRequestString [
	^'Remove it'
]
