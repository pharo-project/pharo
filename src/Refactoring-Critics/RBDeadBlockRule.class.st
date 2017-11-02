"
Dead Block. The block is not assigned, not returned and no message is send to it.
Often this is a left over from using blocks to comment out code.
"
Class {
	#name : #RBDeadBlockRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBDeadBlockRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'DeadBlockRule'
]

{ #category : #accessing }
RBDeadBlockRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBDeadBlockRule >> initialize [
	super initialize.
	self matcher 
		matches: '`{:node | node isBlock and: [node isUsed not]}'
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBDeadBlockRule >> name [
	^ 'Dead Block'
]

{ #category : #accessing }
RBDeadBlockRule >> rationale [
	^ 'Dead Block. The block is not assigned, not returned and no message is send to it.
Often this is a left over from using blocks to comment out code.'
]

{ #category : #accessing }
RBDeadBlockRule >> severity [
	^ #information
]
