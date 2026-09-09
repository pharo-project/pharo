"
I handle user interaction when invoking Inline All Senders Refactoring
"
Class {
	#name : 'ReInlineIntoLocalSendersDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'message'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReInlineIntoLocalSendersDriver class >> driverName [

	^ 'Inline All Senders'
]

{ #category : 'configuration' }
ReInlineIntoLocalSendersDriver >> instantiateRefactoring [

	^ ReInlineIntoLocalSendersRefactoring model: model sendersOf: message in: class
]

{ #category : 'instance creation' }
ReInlineIntoLocalSendersDriver >> scopes: refactoringScopes message: originalMessage class: methodOrigin [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	message := originalMessage.
	class := methodOrigin
]
