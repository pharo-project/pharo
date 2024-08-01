"
Checks ensure:, ifCurtailed:, and showWhile: blocks for assignments or returns that are the last statement in the block. These assignments or returns can be moved outside the block since these messages return the value of the block.
"
Class {
	#name : 'ReAssignmentInBlockRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReAssignmentInBlockRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReAssignmentInBlockRule class >> ruleName [
	^ 'Unnecessary assignment or return in block'
]

{ #category : 'accessing' }
ReAssignmentInBlockRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'AssignmentInBlockRule'
]

{ #category : 'initialization' }
ReAssignmentInBlockRule >> initialize [
	super initialize.
	self  matchesAny: #(
			'`@cursor showWhile: [| `@temps | `@.Statements1. `var := `@object]'
			'`@cursor showWhile: [| `@temps | `@.Statements1. ^`@object]'
			'[| `@temps | `@.Statements. `var := `@object] ensure: `@block'
			'[| `@temps | `@.Statements. ^`@object] ensure: `@block'
			'[| `@temps | `@.Statements. `var := `@object] ifCurtailed: `@block'
			'[| `@temps | `@.Statements. ^`@object] ifCurtailed: `@block' )
]
