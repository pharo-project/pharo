"
Unit tests for queries on methods
"
Class {
	#name : #MethodQueryTest,
	#superclass : #TestCase,
	#category : #'System-Support-Tests'
}

{ #category : #tests }
MethodQueryTest >> testReferencedClasses [
	| refs |
	refs := (CompiledMethod >> #referencedClasses) referencedClasses.
	self assertEmpty: refs.

	refs := thisContext method referencedClasses.
	self assertCollection: refs hasSameElements: {CompiledMethod}
]
