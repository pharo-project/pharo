"
This smell arises when a message is sent by a method,  but no class in the system implements such a message. This method sent will certainly cause a doesNotUnderstand: message when they are executed.
"
Class {
	#name : #RBSentNotImplementedRule,
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
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
RBSentNotImplementedRule >> basicCheck: aMethod [
	^ aMethod messages anySatisfy: [ :each | each isSelectorSymbol not ]
]

{ #category : #running }
RBSentNotImplementedRule >> checkMethod: aMethod [
	| messages |
	messages := aMethod messages reject: [ :each | each isSelectorSymbol].
	messages do: [ :m | result
					   addSearchString: m;
						addMethod: aMethod ]
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
