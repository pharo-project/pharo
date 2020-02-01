"
Checks for ifTrue: or ifFalse: conditions at end of methods that have two or more statements inside their blocks. Such code might better represent the true meaning of the code if they returned self instead.
"
Class {
	#name : #ReGuardingClauseRule,
	#superclass : #ReNodeMatchRule,
	#category : #'Renraku-Rules-Rewrite'
}

{ #category : #accessing }
ReGuardingClauseRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'GuardingClauseRule'
]

{ #category : #accessing }
ReGuardingClauseRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReGuardingClauseRule >> initialize [

	super initialize.
	self addMatchingMethod: 
				'`@MethodName: `@args 
				| `@temps | 
				`@.Statements. 
				`@condition ifTrue: [| `@BlockTemps | `.Statement1. `.Statement2. `@.BStatements]'.
	self addMatchingMethod: 
				'`@MethodName 
				`@.Statements. 
				`@condition ifFalse: [| `@BlockTemps | `.Statement1. `.Statement2. `@.BStatements]'
]

{ #category : #accessing }
ReGuardingClauseRule >> name [
	^ 'Guarding clauses'
]

{ #category : #accessing }
ReGuardingClauseRule >> rationale [
	^ 'Checks for ifTrue: or ifFalse: conditions at end of methods that have two or more statements inside their blocks. Such code might better represent the true meaning of the code if they returned self instead.'
]
