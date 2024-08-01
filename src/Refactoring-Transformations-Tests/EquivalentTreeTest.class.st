Class {
	#name : 'EquivalentTreeTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
EquivalentTreeTest >> setUp [

	super setUp.
	model := RBNamespace onEnvironment: (RBClassEnvironment classes: {
				          Object.
				          RBEquivalentMethodRootHolder.
				          RBEquivalentMethodSubclassHolder })
]

{ #category : 'tests' }
EquivalentTreeTest >> testAllMethodsInHierarchy [

	| checker allMethods  |
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
EquivalentTreeTest >> testEquivalentTreeWithMethodContainingSuperSendInSameClassExpectFound [

	| checker methodNode equivalentTree |

	checker := EquivalentTreeChecker new.
	checker model: model.
	checker on: RBEquivalentMethodSubclassHolder.
	checker extractedFromSelector: #methodToExtractFrom.
	"Here we have equivalent method in the same class as #methodToExtractFrom, and we can replace with it"
	methodNode := RBParser parseMethod: 'testtest ^ 7 raisedTo: super someNumber'.
	equivalentTree := checker findEquivalentTreeFor: methodNode.
	self assert: equivalentTree isNotNil
]

{ #category : 'tests' }
EquivalentTreeTest >> testEquivalentTreeWithMethodHasSuperSendInSuperClassExpectNil [

	| checker methodNode equivalentTree |

	checker := EquivalentTreeChecker new.
	checker model: model.
	checker on: RBEquivalentMethodSubclassHolder.
	checker extractedFromSelector: #methodToExtractFromWithSuperAndSimilarMethodInSuper.
	"Thre is equivalent method, but it is in super class, and we cannot replace it since 
	it has super sends and we're not sure that it will preserve MRO."
	methodNode := RBParser parseMethod: 'testtest ^ 11 raisedTo: super someNumber factorial'.
	equivalentTree := checker findEquivalentTreeFor: methodNode.
	self assert: equivalentTree isNil
]

{ #category : 'tests' }
EquivalentTreeTest >> testEquivalentTreeWithMethodNodeWhenUniqueMethodExpectNil [

	| checker  methodNode equivalentTree |
	checker := EquivalentTreeChecker new.
	checker model: model.
	checker on: RBEquivalentMethodSubclassHolder.
	checker extractedFromSelector: #simpleLocalMethodReturn.
	"There is no method similar to this one."
	methodNode := RBParser parseMethod: 'testMethod: anArg ^ anArg + 7 raisedTo: 2'.
	equivalentTree := checker findEquivalentTreeFor: methodNode.
	
	self assert: equivalentTree isNil

]

{ #category : 'tests' }
EquivalentTreeTest >> testEquivalentTreeWithMethodNodeWithDuplicateMethodExpectFound [

	| checker methodNode equivalentTree |

	checker := EquivalentTreeChecker new.
	checker model: model.
	checker on: RBEquivalentMethodSubclassHolder.
	checker extractedFromSelector: #simpleLocalMethodReturn.
	"Equivalent method exists in EquivalentMethodSubclassHolder superclass"
	methodNode := RBParser parseMethod: 'testtest ^ 42'.
	equivalentTree := checker findEquivalentTreeFor: methodNode.
	self assert: equivalentTree isNotNil
]

{ #category : 'tests' }
EquivalentTreeTest >> testMethodsToBeCheckedExceptSelector [

	| checker methodsToBeChecked |
	
	checker := EquivalentTreeChecker new.
	checker model: model.
	checker on: RBEquivalentMethodSubclassHolder.
	checker extractedFromSelector: #simpleLocalMethodReturn.
	
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
