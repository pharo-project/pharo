"
This smell arises when a message is sent by a method,  but no class in the system implements such a message. This method sent will certainly cause a doesNotUnderstand: message when they are executed.
"
Class {
	#name : #ReSentNotImplementedRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReSentNotImplementedRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReSentNotImplementedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SentNotImplementedRule'
]

{ #category : #running }
ReSentNotImplementedRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemSends |
	problemSends := aMethod sendNodes reject: [ :msgSend | 
		                msgSend selector isSelectorSymbol ].
	problemSends do: [ :msgSend | 
		aCriticBlock cull: (self
				 createTrivialCritiqueOn: aMethod
				 intervalOf: msgSend
				 hint: msgSend selector asString) ]
]

{ #category : #accessing }
ReSentNotImplementedRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
ReSentNotImplementedRule >> name [
	^ 'Messages sent but not implemented'
]

{ #category : #accessing }
ReSentNotImplementedRule >> rationale [
	^ 'Checks for messages that are sent by a method, but no class in the system implements such a message. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.'
]

{ #category : #accessing }
ReSentNotImplementedRule >> severity [
	^ #error
]
