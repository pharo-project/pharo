"
Checks ensure:, ifCurtailed:, and showWhile: blocks for assignments or returns that are the last statement in the block. These assignments or returns can be moved outside the block since these messages return the value of the block.
"
Class {
	#name : #RBAssignmentInBlockRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBAssignmentInBlockRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AssignmentInBlockRule'
]

{ #category : #accessing }
RBAssignmentInBlockRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBAssignmentInBlockRule >> initialize [
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'`@cursor showWhile: [| `@temps | `@.Statements1. `var := `@object]'
			'`@cursor showWhile: [| `@temps | `@.Statements1. ^`@object]'
			'[| `@temps | `@.Statements. `var := `@object] ensure: `@block'
			'[| `@temps | `@.Statements. ^`@object] ensure: `@block'
			'[| `@temps | `@.Statements. `var := `@object] ifCurtailed: `@block'
			'[| `@temps | `@.Statements. ^`@object] ifCurtailed: `@block' )
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBAssignmentInBlockRule >> name [
	^ 'Unnecessary assignment or return in block'
]

{ #category : #accessing }
RBAssignmentInBlockRule >> rationale [
	^ 'Checks ensure:, ifCurtailed:, and showWhile: blocks for assignments or returns that are the last statement in the block. These assignments or returns can be moved outside the block since these messages return the value of the block.'
]
