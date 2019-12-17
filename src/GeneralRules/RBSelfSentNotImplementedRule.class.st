"
This smell arises when a message is sent to self by a method,  but no class in the superclass chain implements such a message. This method sent will certainly cause a doesNotUnderstand: message when they are executed.
"
Class {
	#name : #RBSelfSentNotImplementedRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBSelfSentNotImplementedRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBSelfSentNotImplementedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SelfSentNotImplementedRule'
]

{ #category : #running }
RBSelfSentNotImplementedRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemSends |
	aMethod methodClass isTrait ifTrue: [ ^ self ].
	problemSends := 
		(aMethod sendNodes select: [ :msgSend | msgSend isSelfSend  | msgSend isSuperSend ]) 
			reject: [:msgSend | aMethod methodClass canUnderstand: (msgSend selector)].
	
	problemSends do: [ :msgSend |
			aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: msgSend hint: msgSend selector asString)]
]

{ #category : #accessing }
RBSelfSentNotImplementedRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
RBSelfSentNotImplementedRule >> name [
	^ 'Super and Self Messages sent but not implemented'
]

{ #category : #accessing }
RBSelfSentNotImplementedRule >> rationale [
	^ 'Checks if messages sent to self or super exist in the hierarchy, since these can be statically typed. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.'
]

{ #category : #accessing }
RBSelfSentNotImplementedRule >> severity [
	^ #error
]
