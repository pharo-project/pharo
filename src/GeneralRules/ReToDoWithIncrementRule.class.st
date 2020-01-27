"
Checks for users of to:do: that also increment or decrement a counter.
In Pharo you do not increment or decrement counter but should use the message to:do:by:.

1 to: 100 by: 3 do: [ :each | ... ]
"
Class {
	#name : #ReToDoWithIncrementRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReToDoWithIncrementRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ToDoWithIncrementRule'
]

{ #category : #accessing }
ReToDoWithIncrementRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReToDoWithIncrementRule >> initialize [
	super initialize.
	self matchesAny: #(
			'`@i to: `@j do: [:`e | | `@temps | `@.Stmts. `x := `x + 1. `@.Stmts2]'
			'`@i to: `@j by: `@k do: [:`e | | `@temps | `@.Stmts. `x := `x + `@k. `@.Stmts2]' 
			'`@i to: `@j do: [:`e | | `@temps | `@.Stmts. `x := `x - 1. `@.Stmts2]'
			'`@i to: `@j by: `@k do: [:`e | | `@temps | `@.Stmts. `x := `x - `@k. `@.Stmts2]')
]

{ #category : #accessing }
ReToDoWithIncrementRule >> name [
	^ 'to:do: loop also increments a counter'
]

{ #category : #accessing }
ReToDoWithIncrementRule >> rationale [
	^ 'Checks for users of to:do: that also increment or decrement a counter.'
]
