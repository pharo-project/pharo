"
Checks for messages that are sent but not implemented by a global. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.
"
Class {
	#name : #RBSendsUnknownMessageToGlobalRule,
	#superclass : #ReNodeBasedRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBSendsUnknownMessageToGlobalRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SendsUnknownMessageToGlobalRule'
]

{ #category : #running }
RBSendsUnknownMessageToGlobalRule >> check: aNode forCritiquesDo: aCriticBlock [
	| messageReceiver |

	aNode isMessage ifFalse: [ ^ self ].
	
	messageReceiver := aNode receiver.
	messageReceiver isVariable ifFalse: [ ^ self ].
	

	((Smalltalk globals
		at: messageReceiver name asSymbol
		ifAbsent: [ ^ self ])
			respondsTo: aNode selector)
				ifTrue: [ ^ self ].
		
	aCriticBlock cull: (self critiqueFor: aNode)
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
