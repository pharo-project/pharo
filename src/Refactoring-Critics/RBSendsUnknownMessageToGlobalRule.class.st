"
Checks for messages that are sent but not implemented by a global. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.
"
Class {
	#name : #RBSendsUnknownMessageToGlobalRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBSendsUnknownMessageToGlobalRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SendsUnknownMessageToGlobalRule'
]

{ #category : #accessing }
RBSendsUnknownMessageToGlobalRule >> group [
	^ 'Bugs'
]

{ #category : #initialization }
RBSendsUnknownMessageToGlobalRule >> initialize [
	super initialize.
	self matcher 
		matches: '`{:node :context | node isVariable and: [ Smalltalk includesKey: node name asSymbol ] } `@message: `@args'
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					| what |
					what := Smalltalk globals at: node receiver name asSymbol.
					(what notNil and: [ (what respondsTo: node selector) not ]) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]
]

{ #category : #accessing }
RBSendsUnknownMessageToGlobalRule >> name [
	^ 'Sends unknown message to global'
]

{ #category : #accessing }
RBSendsUnknownMessageToGlobalRule >> severity [
	^ #error
]
