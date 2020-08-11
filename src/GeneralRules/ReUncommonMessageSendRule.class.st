"
Sending messages with a common literal (e.g. ""Object self"") or an uppercase selector name are usually bugs, introduced through missing statement separators.

Notice, problems of this kind will most likely also cause RBSentNotImplementedRule to be invoked, because one should not use such method names in the first place.
"
Class {
	#name : #ReUncommonMessageSendRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReUncommonMessageSendRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReUncommonMessageSendRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UncommonMessageSendRule'
]

{ #category : #running }
ReUncommonMessageSendRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemSelectors |
	problemSelectors := 
		aMethod sendNodes select: [ :msgSend | 
			msgSend selector first isUppercase | (self commonLiterals includes: msgSend selector) ].
	
	problemSelectors do: [ :msgSend |
			aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: msgSend hint: msgSend selector asString) ]
]

{ #category : #accessing }
ReUncommonMessageSendRule >> commonLiterals [

	^ #(#self #super #thisContext #true #false #nil)
]

{ #category : #accessing }
ReUncommonMessageSendRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReUncommonMessageSendRule >> name [
	^ 'Uncommon message send'
]
