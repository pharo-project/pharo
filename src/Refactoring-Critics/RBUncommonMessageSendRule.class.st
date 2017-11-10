"
Sending messages with a common literal (e.g. ""Object self"") or an uppercase selector name are usually bugs, introduced through missing statement separators.
"
Class {
	#name : #RBUncommonMessageSendRule,
	#superclass : #RBBlockLintRule,
	#instVars : [
		'literalNames'
	],
	#category : #Refactoring-Critics-BlockRules
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
RBUncommonMessageSendRule >> basicCheck: aMethod [ 
	^ aMethod messages anySatisfy: [ :each |
		(each isEmpty or: [ each first isUppercase or: [ literalNames includes: each ] ]) ]
]

{ #category : #running }
RBUncommonMessageSendRule >> checkMethod: aMethod [ 
	aMethod messages do: [ :each |
		(each isEmpty or: [ (self isUnknownCapitalizedMessage: each) or: [ literalNames includes: each ] ]) ifTrue: [
			result 
				addMethod: aMethod;
				addSearchString: each ] ]
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
