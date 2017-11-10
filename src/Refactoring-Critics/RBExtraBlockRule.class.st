"
Check for blocks that are immediately evaluated. Since the block is immediately evaluated, there is no need for the statements to be in a block.
For example, [:x | 1 + x] value: 4 is equivalent to 1 + 4
"
Class {
	#name : #RBExtraBlockRule,
	#superclass : #RBParseTreeLintRule,
	#category : #Refactoring-Critics-ParseTreeRules
}

{ #category : #accessing }
RBExtraBlockRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ExtraBlockRule'
]

{ #category : #accessing }
RBExtraBlockRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBExtraBlockRule >> initialize [
	super initialize.
	self matcher 
		matches: '`{:node | node isMessage and: [node receiver isBlock and: [node parent isCascade not and: [#(#value #value: #value:value: #value:value:value: #valueWithArguments:) includes: node selector]]]}'
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBExtraBlockRule >> name [
	^ 'Block immediately evaluated'
]

{ #category : #accessing }
RBExtraBlockRule >> rationale [
	^ 'Check for blocks that are immediately evaluated.'
]

{ #category : #accessing }
RBExtraBlockRule >> severity [
	^ #information
]
