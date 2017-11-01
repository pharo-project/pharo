"
Checks for users of to:do: when the shorter collect: would work.
"
Class {
	#name : #RBToDoCollectRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBToDoCollectRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ToDoCollectRule'
]

{ #category : #accessing }
RBToDoCollectRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBToDoCollectRule >> initialize [
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'| `@temps1 |
			`@.Stmts1.
			`collection := Array new: `@size.
			`@.Stmts2.
			1 to: `@size do: 
					[:`i | 
					| `@Btemps2 | 
					`@.BStmts1. 
					`collection at: `i put: `@obj.
					`@.BStmt2].
			`@.Stmts3'
				'| `@temps1 |
			`@.Stmts1.
			`collection := Array new: `@size.
			`@.Stmts2.
			1 to: `collection size do: 
					[:`i | 
					| `@Btemps2 | 
					`@.BStmts1. 
					`collection at: `i put: `@obj.
					`@.BStmt2].
			`@.Stmts3' )
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBToDoCollectRule >> name [
	^ 'to:do: used instead of collect:'
]
