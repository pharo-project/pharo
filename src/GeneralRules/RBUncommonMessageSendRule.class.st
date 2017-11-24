"
Sending messages with a common literal (e.g. ""Object self"") or an uppercase selector name are usually bugs, introduced through missing statement separators.
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
	(aMethod messages anySatisfy: [ :each |
		(each isEmpty or: [ each first isUppercase or: [ literalNames includes: each ] ]) ]) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]
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
