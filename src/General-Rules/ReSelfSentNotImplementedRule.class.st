"
This smell arises when a message is sent to self by a method,  but no class in the superclass chain implements such a message. This method sent will certainly cause a doesNotUnderstand: message when they are executed.
"
Class {
	#name : 'ReSelfSentNotImplementedRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'testing' }
ReSelfSentNotImplementedRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReSelfSentNotImplementedRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
ReSelfSentNotImplementedRule class >> rationale [
	^ 'Checks if messages sent to self or super exist in the hierarchy, since these can be statically typed. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.'
]

{ #category : 'accessing' }
ReSelfSentNotImplementedRule class >> ruleName [
	^ 'Super and Self Messages sent but not implemented'
]

{ #category : 'accessing' }
ReSelfSentNotImplementedRule class >> severity [
	^ #error
]

{ #category : 'accessing' }
ReSelfSentNotImplementedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'SelfSentNotImplementedRule'
]

{ #category : 'running' }
ReSelfSentNotImplementedRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemSends ignored |
	aMethod methodClass isTrait ifTrue: [ ^ self ].
	problemSends :=
		(aMethod sendNodes select: [ :msgSend | msgSend isSelfSend or: [ msgSend isSuperSend ] ])
			reject: [:msgSend | aMethod methodClass canUnderstand: (msgSend selector)].
	
	"if we know that the selector does not exist, we ignore"
	ignored := aMethod allIgnoredNotImplementedSelectors.
	problemSends := problemSends reject: [:each | ignored includes: each selector].
	
	problemSends do: [ :msgSend |
			aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: msgSend hint: msgSend selector asString)]
]
