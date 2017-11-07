"
This smell arises when a message is sent by a method,  but no class in the system implements such a message. This method sent will certainly cause a doesNotUnderstand: message when they are executed.
"
Class {
	#name : #RBSentNotImplementedRule,
	#superclass : #ReAbstractRule,
	#category : 'GeneralRules-Migrated'
}

{ #category : #testing }
RBSentNotImplementedRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBSentNotImplementedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SentNotImplementedRule'
]

{ #category : #running }
RBSentNotImplementedRule >> check: aMethod forCritiquesDo: aCriticBlock [

	aMethod messages
		select: [ :each | each isSelectorSymbol not ]
		thenDo: [ :each |
			aCriticBlock cull: (ReTrivialCritique
				withAnchor: (ReSearchStringSourceAnchor
					entity: aMethod
					string: each)
				by: self
				hint: each) ]
		
]

{ #category : #accessing }
RBSentNotImplementedRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
RBSentNotImplementedRule >> name [
	^ 'Messages sent but not implemented'
]

{ #category : #accessing }
RBSentNotImplementedRule >> rationale [
	^ 'Checks for messages that are sent by a method, but no class in the system implements such a message. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.'
]

{ #category : #accessing }
RBSentNotImplementedRule >> severity [
	^ #error
]
