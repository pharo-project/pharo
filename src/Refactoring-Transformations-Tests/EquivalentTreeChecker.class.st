Class {
	#name : 'EquivalentTreeChecker',
	#superclass : 'Object',
	#instVars : [
		'class',
		'model',
		'extractedFromSelector'
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
EquivalentTreeChecker >> checkIfMethodNode: aMethodNode isEquivalentMethodNode: anAnotherMethodNode [

	| anotherMethodsClass |
	"Methods should have the same number of arguments"
	aMethodNode arguments size = anAnotherMethodNode arguments size ifFalse: [
		^ false ].
	self flag: #todo. "if I don't need return I can be equivalent to a method that has return; check code below"
	"needsReturn ifFalse: [ tree := self removeReturnsOf: tree ]."
	"Methods bodies should be the same with the exception of argument names"
	(anAnotherMethodNode body
		 equalTo: aMethodNode body
		 exceptForVariables:
		 (anAnotherMethodNode arguments collect: [ :each | each name ]))
		ifFalse: [ ^ false ].
	"Now we know methods are equivalent, we now check if it has super sends,
	if it does, we cannot guarantee behavior preservation if that method is
	from one of the superclasses (since super will not call the same method
	as it would call when it was inlined in `aMethodNode`)"
	self flag: #todo. "improve MRO preservation when super calls are present"
	anotherMethodsClass := class whichClassIncludesSelector: anAnotherMethodNode selector.
	(anAnotherMethodNode superMessages isNotEmpty 
			and: [ class name ~= anotherMethodsClass name ])
		ifTrue: [ ^ false ].
	^ true
]

{ #category : 'accessing' }
EquivalentTreeChecker >> extractedFromSelector: aString [ 
	"We have to remember the methods containing the original extracted expression,
	so that it is excluded from the search."

	extractedFromSelector := aString
]

{ #category : 'executing' }
EquivalentTreeChecker >> findEquivalentTreeFor: aMethodNode [

	| equivalentMethods |
	equivalentMethods := self methodsToBeChecked select: [ :method |
		                     self
			                     checkIfMethodNode: aMethodNode 
			                     isEquivalentMethodNode: method ast ].
	equivalentMethods ifNotEmpty: [ ^ equivalentMethods first ].
	^ nil
]

{ #category : 'query' }
EquivalentTreeChecker >> methodsToBeChecked [

	^ self allMethodsInHierarchy reject: [ :m | m selector = extractedFromSelector ]
]

{ #category : 'instance creation' }
EquivalentTreeChecker >> model: aModel [ 
	
	model := aModel
]

{ #category : 'instance creation' }
EquivalentTreeChecker >> on: aClass [ 
	
	class := model classNamed: aClass name
]
