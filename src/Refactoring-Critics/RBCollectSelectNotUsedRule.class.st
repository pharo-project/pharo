"
Checks for senders of typical collection enumeration methods that return an unused result.

For example, check that the result of a select: or detect: is used.
"
Class {
	#name : #RBCollectSelectNotUsedRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBCollectSelectNotUsedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'CollectSelectNotUsedRule'
]

{ #category : #accessing }
RBCollectSelectNotUsedRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBCollectSelectNotUsedRule >> initialize [
	super initialize.
	self matcher 
		matches: '`{:node | node isMessage and: [(#(#select: #collect: #reject:) includes: node selector) and: [node isUsed not]]}'
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBCollectSelectNotUsedRule >> name [
	^ 'Doesn''t use the result of a collect:/select:'
]

{ #category : #accessing }
RBCollectSelectNotUsedRule >> rationale [
	^ 'Checks for senders of typical collection enumeration methods that return an unused result.'
]
