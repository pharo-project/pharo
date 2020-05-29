"
Checks for messages that are sent but not implemented by a global. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.
"
Class {
	#name : #ReSendsUnknownMessageToGlobalRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReSendsUnknownMessageToGlobalRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SendsUnknownMessageToGlobalRule'
]

{ #category : #running }
ReSendsUnknownMessageToGlobalRule >> basicCheck: aNode [
	| messageReceiver |
	aNode isMessage ifFalse: [ ^ false ].

	messageReceiver := aNode receiver.
	messageReceiver isVariable ifFalse: [ ^ false ].

	^ ((Smalltalk globals at: messageReceiver name asSymbol ifAbsent: [ ^ false ]) respondsTo: aNode selector) not
]

{ #category : #accessing }
ReSendsUnknownMessageToGlobalRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
ReSendsUnknownMessageToGlobalRule >> name [
	^ 'Sends unknown message to global'
]

{ #category : #accessing }
ReSendsUnknownMessageToGlobalRule >> severity [
	^ #error
]
