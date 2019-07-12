"
Checks for messages that are sent but not implemented by a global. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.
"
Class {
	#name : #RBSendsUnknownMessageToGlobalRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBSendsUnknownMessageToGlobalRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SendsUnknownMessageToGlobalRule'
]

{ #category : #running }
RBSendsUnknownMessageToGlobalRule >> basicCheck: aNode [
	| messageReceiver |
	aNode isMessage ifFalse: [ ^ false ].

	messageReceiver := aNode receiver.
	messageReceiver isVariable ifFalse: [ ^ false ].

	^ ((Smalltalk globals at: messageReceiver name asSymbol ifAbsent: [ ^ false ]) respondsTo: aNode selector) not
]

{ #category : #accessing }
RBSendsUnknownMessageToGlobalRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
RBSendsUnknownMessageToGlobalRule >> name [
	^ 'Sends unknown message to global'
]

{ #category : #accessing }
RBSendsUnknownMessageToGlobalRule >> severity [
	^ #error
]
