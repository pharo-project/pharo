"
Checks for senders of comparator messages that do not use the result of the comparison.
"
Class {
	#name : #RBEqualNotUsedRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBEqualNotUsedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'EqualNotUsedRule'
]

{ #category : #accessing }
RBEqualNotUsedRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBEqualNotUsedRule >> initialize [
	super initialize.
	self matcher 
		matches: '`{:node | node isMessage and: [node isUsed not and: [#(#= #== #~= #~~ #< #> #<= #>=) includes: node selector]]}'
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBEqualNotUsedRule >> name [
	^ 'Doesn''t use the result of a =, ~=, etc.'
]
