"
Transforms single branch conditionals with multi-statement bodies into a sequence of statements guarded by a conditional return.
For example

[[[ 
foo 
	statements. 
	condition ifTrue: [ statement1. statement2 ]
]]]

is transformed into 

[[[  
foo
	statements.
	condition ifFalse: [^self].
	statement1.
	statement2.
]]]
"
Class {
	#name : #GRGuardClauseRule,
	#superclass : #ReNodeRewriteRule,
	#category : #GeneralRules
}

{ #category : #accessing }
GRGuardClauseRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
GRGuardClauseRule >> initialize [
	super initialize.
	self 
		addMatchingMethod: '`@methodName: `@args 
					| `@temps | 
					`@.Statements. 
					`@condition ifTrue: [| `@trueTemps | `.Statement1. `.Statement2. `@.Statements1]'
		rewriteTo: '`@methodName: `@args
					| `@temps `@trueTemps |
					`@.Statements.
					`@condition ifFalse: [^self].
					`.Statement1.
					`.Statement2.
					`@.Statements1';
		addMatchingMethod: '`@methodName: `@args 
					| `@temps | 
					`@.Statements. 
					`@condition ifFalse: [| `@falseTemps | `.Statement1. `.Statement2. `@.Statements1]'
		rewriteTo: '`@methodName: `@args
					| `@temps `@falseTemps |
					`@.Statements.
					`@condition ifTrue: [^self].
					`.Statement1.
					`.Statement2.
					`@.Statements1'
]

{ #category : #accessing }
GRGuardClauseRule >> name [
	^ 'Replace single branch conditional with guard clause'
]
