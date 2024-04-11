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
EquivalentTreeTest >> testEquivalentTreeWithMethodNodeWhenUniqueMethodExpectNil [

	| checker  methodNode equivlentTree |
	checker := EquivalentTreeChecker new.
	checker model: RBNamespace new.
	checker on: RBEquivalentMethodSubclassHolder.
	checker selector: #simpleLocalMethodReturn.
	
	methodNode := RBParser parseMethod: 'testMethod: anArg ^ anArg + 7 raisedTo: 2'.
	equivlentTree := checker checkEquivalentTreeFor: methodNode.
	
	self assert: equivlentTree isNil

]

{ #category : 'tests' }
EquivalentTreeTest >> testEquivalentTreeWithMethodNodeWithDuplicateMethodExpectFound [

	| checker methodNode equivlentTree |

	checker := EquivalentTreeChecker new.
	checker model: RBNamespace new.
	checker on: RBEquivalentMethodSubclassHolder.
	checker selector: #simpleLocalMethodReturn.

	methodNode := RBParser parseMethod: 'testtest ^ 42'.
	equivlentTree := checker checkEquivalentTreeFor: methodNode.
	self assert: equivlentTree isNotNil
]

{ #category : 'tests' }
EquivalentTreeTest >> testMethodsToBeCheckedExceptSelector [

	| checker methodsToBeChecked model |
	model := RBNamespace new.
	
	checker := EquivalentTreeChecker new.
	checker model: model.
	checker on: RBEquivalentMethodSubclassHolder.
	checker selector: #simpleLocalMethodReturn.
	
	methodsToBeChecked := checker methodsToBeChecked.
	
	"pay attention that we want to have rbMethods"
	"pay attention rb-Methods are not cached so the includes: did not find them."
	self assert:
			((methodsToBeChecked collect: [:each | each selector ])
		includesAll: 
			((model classNamed: RBEquivalentMethodSubclassHolder name) methods 
				collect: [:each | each selector] 
				thenReject: [:each | each = #simpleLocalMethodReturn ])).
	
	self assert:
			((methodsToBeChecked collect: [:each | each selector ])
		includesAll: 
			((model classNamed: RBEquivalentMethodSubclassHolder superclass name) methods 
				collect: [:each | each selector]))
]
