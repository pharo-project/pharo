"
I provide a specialized context description in the case where a test failure was found on the stack of the context.

I reference that test  failure.

"
Class {
	#name : #StDebuggerTestFailureContextPredicate,
	#superclass : #StDebuggerContextPredicate,
	#instVars : [
		'failure'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #testing }
StDebuggerTestFailureContextPredicate >> contextSignalsException [
	^true
]

{ #category : #accessing }
StDebuggerTestFailureContextPredicate >> failure: anObject [
	failure := anObject
]

{ #category : #predicates }
StDebuggerTestFailureContextPredicate >> isContextAnAssertionFailure [
	^true
]

{ #category : #printing }
StDebuggerTestFailureContextPredicate >> isSteppable [ 
	^false
]

{ #category : #printing }
StDebuggerTestFailureContextPredicate >> printDescription [

	| str |
	str := WriteStream on: String new.
	self printDescriptionPrefixOn: str.

	(failure isKindOf: TestFailure) ifTrue: [ 
		str << failure description.
		^ str contents ].
	(failure isClass and: [ failure includesBehavior: TestFailure ]) 
		ifTrue: [ 
			| variable |
			variable := context temporaryVariableNamed: #aStringOrBlock.
			str << (variable readInContext: context) value.
			^ str contents ].

	str << 'Cannot print test failure in: '.
	str << context printString.
	^ str contents
]
