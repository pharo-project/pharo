Class {
	#name : 'EquivalentTreeTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
EquivalentTreeTest >> testAllMethodsInHierarchy [

	| checker allMethods model |
	model := RBNamespace new.
	checker := EquivalentTreeChecker new.
	checker model: model.
	checker on: RBEquivalentMethodSubclassHolder.
	
	allMethods := checker allMethodsInHierarchy.
	
	"pay attention that we want to have rbMethods"
	"pay attention rb-Methods are not cached so the includes: did not find them."
	self assert:
		((allMethods collect: [:each | each selector ])
			includesAll: ((model classNamed: RBEquivalentMethodSubclassHolder name) methods collect: [:each | each selector])).
	
	self assert:
		((allMethods collect: [:each | each selector ])
			includesAll: ((model classNamed: RBEquivalentMethodSubclassHolder superclass name) methods collect: [:each | each selector])).
			
			
	"since we did redefine supermethods, none of Object should be listed."
	self deny:
		((allMethods collect: [:each | each selector ])
			includesAll: ((model classNamed: Object name) methods collect: [:each | each selector])).
]

{ #category : 'tests' }
EquivalentTreeTest >> testSimpleLocalMethodReturn [

	| checker|
	checker := EquivalentTreeChecker new.
	checker checkIfMethod: RBEquivalentMethodSubclassHolder>>#simpleLocalMethodReturn 
			
	
]
