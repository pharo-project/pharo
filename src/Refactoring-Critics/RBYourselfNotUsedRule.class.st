"
Check for methods sending the yourself message when it is not necessary.
"
Class {
	#name : #RBYourselfNotUsedRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBYourselfNotUsedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'YourselfNotUsedRule'
]

{ #category : #accessing }
RBYourselfNotUsedRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBYourselfNotUsedRule >> initialize [
	super initialize.
	self matcher 
		matches: '`{:node | node parent isUsed not} yourself'
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBYourselfNotUsedRule >> name [
	^ 'Doesn''t use the result of a yourself message'
]

{ #category : #accessing }
RBYourselfNotUsedRule >> rationale [
	^ 'Check for methods sending the yourself message when it is not necessary.'
]
