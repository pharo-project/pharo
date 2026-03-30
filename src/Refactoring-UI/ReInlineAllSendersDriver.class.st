"
I handle user interaction when invoking Inline All Senders Refactoring
"
Class {
	#name : 'ReInlineAllSendersDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'message',
		'class'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'configuration' }
ReInlineAllSendersDriver >> instantiateRefactoring [

	^ ReInlineAllSendersRefactoring model: model sendersOf: message in: class
]

{ #category : 'execution' }
ReInlineAllSendersDriver >> runRefactoring [

	self configureRefactoring.

	refactoring failedApplicabilityPreconditions ifNotEmpty: [ :conditions |
			self informConditions: conditions.
			^ false ].
	self applyChanges
]

{ #category : 'instance creation' }
ReInlineAllSendersDriver >> scopes: refactoringScopes message: originalMessage class: methodOrigin [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	message := originalMessage.
	class := methodOrigin
]
