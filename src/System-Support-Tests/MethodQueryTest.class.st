Class {
	#name : #MethodQueryTest,
	#superclass : #TestCase,
	#category : #'System-Support-Tests'
}

{ #category : #tests }
MethodQueryTest >> testReferencedClasses [
	| refs |
	
	refs := (CompiledMethod>>#referencedClasses) referencedClasses.
	self assert: refs isEmpty.
	
	refs := thisContext method referencedClasses.
	self assert: refs equals: { CompiledMethod }.
	
	
]
