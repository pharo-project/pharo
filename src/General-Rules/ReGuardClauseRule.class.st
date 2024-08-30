"
Checks for ifTrue: or ifFalse: conditions at end of methods that have two or more statements inside their blocks. Such code might better represent the true meaning of the code if they returned self instead.

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
	#name : 'ReGuardClauseRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReGuardClauseRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReGuardClauseRule class >> rationale [
	^ 'Checks for ifTrue: or ifFalse: conditions at end of methods that have two or more statements inside their blocks. Such code might better represent the true meaning of the code if they returned self instead.'
]

{ #category : 'accessing' }
ReGuardClauseRule class >> ruleName [
	^ 'Replace single branch conditional with guard clause'
]

{ #category : 'initialization' }
ReGuardClauseRule >> initialize [
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
