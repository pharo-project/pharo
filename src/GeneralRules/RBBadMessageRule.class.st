"
This smell arises when methods send messages that perform low level things. You might want to limit the number of such messages in your application. Messages such as #isKindOf: can signify a lack of polymorphism. You can see which methods are ""questionable"" by editing the RBBadMessageRule>>badSelectors method. Some examples are: #respondsTo: #isMemberOf: #performMethod: and #performMethod:arguments:

This rule is not active for methods in test classes.
"
Class {
	#name : #RBBadMessageRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBBadMessageRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'BadMessageRule2'
]

{ #category : #private }
RBBadMessageRule >> badSelectors [
	^ #( #become: #isKindOf: #respondsTo: #isMemberOf: #perform: #perform:arguments: #perform:with: #perform:with:with: #perform:with:with:with: #allOwners #instVarAt: #instVarAt:put: #nextInstance instVarsInclude: #nextObject caseOf: caseOf:otherwise: caseError isThisEverCalled isThisEverCalled: becomeForward: instVarNamed: instVarNamed:put: someObject primitiveChangeClassTo:)
]

{ #category : #enumerating }
RBBadMessageRule >> basicCheck: aNode [
	aNode isMessage ifFalse: [ ^false ].
	"In tests using these messages (like isKindOf:) is no problem"
	aNode methodNode methodClass isTestCase ifTrue: [ ^false ].
	^ self badSelectors includes: aNode selector
]

{ #category : #accessing }
RBBadMessageRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
RBBadMessageRule >> name [
	^ 'Sends "questionable" message'
]

{ #category : #accessing }
RBBadMessageRule >> rationale [
	^ 'Check methods that send messages that perform low level things.'
]
