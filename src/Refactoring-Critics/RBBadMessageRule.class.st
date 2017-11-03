"
This smell arises when methods send messages that perform low level things. You might want to limit the number of such messages in your application. Messages such as #isKindOf: can signify a lack of polymorphism. You can see which methods are ""questionable"" by editing the RBBadMessageRule>>badSelectors method. Some examples are: #respondsTo: #isMemberOf: #performMethod: and #performMethod:arguments:
"
Class {
	#name : #RBBadMessageRule,
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
}

{ #category : #testing }
RBBadMessageRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBBadMessageRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'BadMessageRule'
]

{ #category : #private }
RBBadMessageRule >> badSelectors [
	^ #( #become: #isKindOf: #respondsTo: #isMemberOf: #perform: #perform:arguments: #perform:with: #perform:with:with: #perform:with:with:with: #allOwners #instVarAt: #instVarAt:put: #nextInstance instVarsInclude: #nextObject caseOf: caseOf:otherwise: caseError isThisEverCalled isThisEverCalled: becomeForward: instVarNamed: instVarNamed:put: someObject primitiveChangeClassTo:)
]

{ #category : #running }
RBBadMessageRule >> basicCheck: aMethod [ 

	^ aMethod sendsAnySelectorOf: self badSelectors
]

{ #category : #running }
RBBadMessageRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) ifTrue: [
		result
			addMethod: aMethod;
			searchStrings: self badSelectors ]
	
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
