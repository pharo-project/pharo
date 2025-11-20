Class {
	#name : 'ReSharedVariableHasReferencesTest',
	#superclass : 'ReVariableHasReferencesTest',
	#classVars : [
		'UnreferencedVar'
	],
	#category : 'Refactoring-Core-Tests-Model',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Model'
}

{ #category : 'accessing' }
ReSharedVariableHasReferencesTest >> classToTest [

	^ ReSharedVariableHasReferences
]

{ #category : 'accessing' }
ReSharedVariableHasReferencesTest >> initialize [
	<ignoreUnusedVariables: #( #UnreferencedVar )>
	super initialize
]

{ #category : 'tests' }
ReSharedVariableHasReferencesTest >> testHierarchyOfDoesNotReferencesSharedVariableFromClassSideWithNegation [

	| root condInst condClss condInstNot condClssNot |
		
	root := self model classNamed: self class name.
	self assert: (root definesClassVariable: #UnreferencedVar).
	
	condInst := (self classToTest new
			 				hierarchyOf: root
			 				referencesSharedVariable: #UnreferencedVar) not.
	condClss := (self classToTest new
			 				hierarchyOf: root classSide
			 				referencesSharedVariable: #UnreferencedVar) not.
							
	self deny: condInst check.
	self deny: condClss check.
	
	self assert: condInst violators equals: {#UnreferencedVar}.
	self assert: condClss violators equals: {#UnreferencedVar}.
	
	condInstNot := condInst not.
	condClssNot := condClss not.
	
	self assert: condInstNot check.
	self assert: condClssNot check.
	
	self assert: condInstNot violators isEmpty.
	self assert: condClssNot violators isEmpty.
]

{ #category : 'tests' }
ReSharedVariableHasReferencesTest >> testHierarchyOfReferencesSharedVariableFromClassSide [

	| root sub |
		
	root := self model classNamed: #MyClassAlpha.
	sub := self model classNamed: #MyClassBeta.
	self assert: (root subclasses includes: sub).
	self assert: (root definesClassVariable: #SharedVarA).
	self assert: (sub definesClassVariable: #SharedVarA).
	
	self assert: (self classToTest new
			 				hierarchyOf: root
			 				referencesSharedVariable: #SharedVarA) check.
	self assert: (self classToTest new
			 				hierarchyOf: root classSide
			 				referencesSharedVariable: #SharedVarA) check.
]

{ #category : 'tests' }
ReSharedVariableHasReferencesTest >> testHierarchyOfReferencesSharedVariableFromClassSideWithNegation [

	| root sub condInst condClss condInstNot condClssNot |
		
	root := self model classNamed: #MyClassAlpha.
	sub := self model classNamed: #MyClassBeta.
	self assert: (root subclasses includes: sub).
	self assert: (root definesClassVariable: #SharedVarA).
	self assert: (sub definesClassVariable: #SharedVarA).
	
	condInst := self classToTest new
			 				hierarchyOf: root
			 				referencesSharedVariable: #SharedVarA.
	condClss := self classToTest new
			 				hierarchyOf: root classSide
			 				referencesSharedVariable: #SharedVarA.
							
	self assert: condInst check.
	self assert: condClss check.
	
	self assert: condInst violators isEmpty.
	self assert: condClss violators isEmpty.
	
	condInstNot := condInst not.
	condClssNot := condClss not.
	
	self deny: condInstNot check.
	self deny: condClssNot check.
	
	self assert: condInstNot violators equals: { #SharedVarA }.
	self assert: condClssNot violators equals: { #SharedVarA }.
]
