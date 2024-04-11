Class {
	#name : 'EquivalentTreeChecker',
	#superclass : 'Object',
	#instVars : [
		'selector',
		'class',
		'model'
	],
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'query' }
EquivalentTreeChecker >> allMethodsInHierarchy [
	^ ((class withAllSuperclassesUntil: Object) 
			flatCollect: [ :aClass | aClass methods]) 
]

{ #category : 'executing' }
EquivalentTreeChecker >> checkEquivalentTreeFor: aMethodNode [

	| equivalentMethods |
	equivalentMethods := self methodsToBeChecked select: [ :method |
		                     self
			                     checkIfMethodNode: aMethodNode 
			                     isEquivalentMethodNode: method ast ].
	equivalentMethods ifNotEmpty: [ ^ equivalentMethods first ].
	^ nil
]

{ #category : 'as yet unclassified' }
EquivalentTreeChecker >> checkIfMethodNode: aMethodNode isEquivalentMethodNode: anAnotherMethodNode [

	aMethodNode arguments size = anAnotherMethodNode arguments size ifFalse: [
		^ false ].
	(anAnotherMethodNode body
		 equalTo: aMethodNode body
		 exceptForVariables:
		 (anAnotherMethodNode arguments collect: [ :each | each name ]))
		ifFalse: [ ^ false ].
	^ true
]

{ #category : 'query' }
EquivalentTreeChecker >> methodsToBeChecked [

	^ self allMethodsInHierarchy reject: [ :m | m selector = selector ]
]

{ #category : 'instance creation' }
EquivalentTreeChecker >> model: aModel [ 
	
	model := aModel
]

{ #category : 'instance creation' }
EquivalentTreeChecker >> on: aClass [ 
	
	class := model classNamed: aClass name
]

{ #category : 'accessing' }
EquivalentTreeChecker >> selector: aString [ 
	selector := aString
]
