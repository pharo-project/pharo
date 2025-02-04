"
This smell arises when a message is sent by a method,  but no class in the system implements such a message. This method sent will certainly cause a doesNotUnderstand: message when they are executed.
"
Class {
	#name : 'ReSentNotImplementedRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'testing' }
ReSentNotImplementedRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReSentNotImplementedRule class >> group [
	^ self bugsGroup
]

{ #category : 'accessing' }
ReSentNotImplementedRule class >> rationale [
	^ 'Checks for messages that are sent by a method, but no class in the system implements such a message. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.'
]

{ #category : 'accessing' }
ReSentNotImplementedRule class >> ruleName [
	^ 'Messages sent but not implemented'
]

{ #category : 'accessing' }
ReSentNotImplementedRule class >> severity [
	^ #error
]

{ #category : 'accessing' }
ReSentNotImplementedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'SentNotImplementedRule'
]

{ #category : 'running' }
ReSentNotImplementedRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| ignored problemSends |

	ignored := aMethod allIgnoredNotImplementedSelectors.
	ignored := ignored , #(!= !== <>). "See ReNotEqualityOperatorsRule"

	problemSends := aMethod sendNodes reject: [ :msgSend |
		                msgSend selector isUsedAsSelectorSymbol ].
	problemSends := problemSends reject: [:each | ignored includes: each selector].

	problemSends do: [ :msgSend |
		aCriticBlock cull: (self
				 createTrivialCritiqueOn: aMethod
				 intervalOf: msgSend
				 hint: msgSend selector asString) ]
]
