"
Check for possible uses of the result returned by an add: or addAll: messages. These messages return their arguments not the receiver. As a result, may uses of the results are wrong.
"
Class {
	#name : #RBUsesAddRule,
	#superclass : #RBParseTreeLintRule,
	#category : #Refactoring-Critics-ParseTreeRules
}

{ #category : #accessing }
RBUsesAddRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UsesAddRule'
]

{ #category : #accessing }
RBUsesAddRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBUsesAddRule >> initialize [
	super initialize.
	self matcher 
		matches: '`{:node | node isMessage and: [(node selector == #add: or: [node selector == #addAll:]) and: [node isDirectlyUsed]]}'
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBUsesAddRule >> name [
	^ 'Uses the result of an add: message'
]
