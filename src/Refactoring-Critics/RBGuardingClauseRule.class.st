"
Checks for ifTrue: or ifFalse: conditions at end of methods that have two or more statements inside their blocks. Such code might better represent the true meaning of the code if they returned self instead.
"
Class {
	#name : #RBGuardingClauseRule,
	#superclass : #RBParseTreeLintRule,
	#category : #'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBGuardingClauseRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'GuardingClauseRule'
]

{ #category : #accessing }
RBGuardingClauseRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBGuardingClauseRule >> initialize [

	super initialize.
	self matcher
		matchesAnyMethodOf:
			#('`@MethodName: `@args 
				| `@temps | 
				`@.Statements. 
				`@condition ifTrue: [| `@BlockTemps | `.Statement1. `.Statement2. `@.BStatements]' '`@MethodName: `@args 
				| `@temps | 
				`@.Statements. 
				`@condition ifFalse: [| `@BlockTemps | `.Statement1. `.Statement2. `@.BStatements]')
		do:
			[ :node :answer | answer ifNil: [ node body statements last ] ifNotNil: [ answer ] ]
]

{ #category : #accessing }
RBGuardingClauseRule >> name [
	^ 'Guarding clauses'
]

{ #category : #accessing }
RBGuardingClauseRule >> rationale [
	^ 'Checks for ifTrue: or ifFalse: conditions at end of methods that have two or more statements inside their blocks. Such code might better represent the true meaning of the code if they returned self instead.'
]
