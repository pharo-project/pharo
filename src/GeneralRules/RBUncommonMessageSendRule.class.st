"
Sending messages with a common literal (e.g. ""Object self"") or an uppercase selector name are usually bugs, introduced through missing statement separators.

Notice, problems of this kind will most likely also cause RBSentNotImplementedRule to be invoked, because one should not use such method names in the first place.
"
Class {
	#name : #RBUncommonMessageSendRule,
	#superclass : #ReAbstractRule,
	#instVars : [
		'literalNames'
	],
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBUncommonMessageSendRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBUncommonMessageSendRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UncommonMessageSendRule'
]

{ #category : #running }
RBUncommonMessageSendRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemSelectors |
	problemSelectors := 
		aMethod sendNodes select: [ :msgSend | 
			msgSend selector first isUppercase | (self commonLiterals includes: msgSend selector) ].
	
	problemSelectors do: [ :msgSend |
			aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: msgSend hint: msgSend selector asString) ]
]

{ #category : #accessing }
RBUncommonMessageSendRule >> commonLiterals [

	^ #(#self #super #thisContext #true #false #nil) asIdentitySet
]

{ #category : #accessing }
RBUncommonMessageSendRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBUncommonMessageSendRule >> initialize [
	super initialize.
	literalNames := self commonLiterals.
]

{ #category : #private }
RBUncommonMessageSendRule >> isUnknownCapitalizedMessage: aSelector [
	^ aSelector first isUppercase and: [ aSelector isSelectorSymbol not ]
]

{ #category : #accessing }
RBUncommonMessageSendRule >> name [
	^ 'Uncommon message send'
]
