"
Checks for users of to:do: when the shorter collect: would work.
"
Class {
	#name : 'ReToDoCollectRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReToDoCollectRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReToDoCollectRule class >> ruleName [
	^ 'to:do: used instead of collect:'
]

{ #category : 'accessing' }
ReToDoCollectRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ToDoCollectRule'
]

{ #category : 'initialization' }
ReToDoCollectRule >> initialize [
	super initialize.
	self matchesAny: #(
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
]
