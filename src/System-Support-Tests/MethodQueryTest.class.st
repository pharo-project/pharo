"
Unit tests for queries on methods
"
Class {
	#name : 'MethodQueryTest',
	#superclass : 'TestCase',
	#category : 'System-Support-Tests-Base',
	#package : 'System-Support-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
MethodQueryTest >> testReferencedClasses [

	| refs |
	refs := (CompiledMethod >> #referencedClasses) referencedClasses.
	self assertCollection: refs hasSameElements: {IdentitySet}.

	refs := thisContext method referencedClasses.
	self assertCollection: refs hasSameElements: {CompiledMethod. IdentitySet}
]
