"
I am a refactoring for removing a method and those of its subclasses,
 to remove the methods use RBRemoveMethodRefactoring.

Example
-------
Script
```
(RBRemoveHierarchyMethodRefactoring 
		removeMethods: #(#msg4)
		from: RBSharedPoolForTestData) execute
```
"
Class {
	#name : 'RBRemoveMethodsInHierarchyRefactoring',
	#superclass : 'RBCompositeRefactoring',
	#instVars : [
		'selectors',
		'class'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'removing' }
RBRemoveMethodsInHierarchyRefactoring class >> selectors: aSelectorsCollection from: aClass [ 
	
	^ self new selectors: aSelectorsCollection from: aClass
]

{ #category : 'preconditions' }
RBRemoveMethodsInHierarchyRefactoring >> applicabilityPreconditions [ 

	^ { RBDefinesSelectorsCondition new definesSelectors: selectors in: class }
]

{ #category : 'transforming' }
RBRemoveMethodsInHierarchyRefactoring >> delete: aSelectorsCollection in: aClass [

	| containingMethods |
	containingMethods := aSelectorsCollection select: [ :sel |
		                     aClass realClass includesSelector: sel ].
	containingMethods ifNotEmpty: [
		refactorings add: (RBRemoveMethodsRefactoring
				 model: self model
				 selectors: containingMethods
				 from: aClass) ]
]

{ #category : 'transforming' }
RBRemoveMethodsInHierarchyRefactoring >> initializeRefactorings [

	class withAllSubclasses do: [ :aClass |
		self delete: selectors in: aClass ]
]

{ #category : 'removing' }
RBRemoveMethodsInHierarchyRefactoring >> selectors: aSelectorsCollection from: aClass [

	class := self classObjectFor: aClass.
	selectors := aSelectorsCollection.
	refactorings := OrderedCollection new.
	self initializeRefactorings 
]

{ #category : 'printing' }
RBRemoveMethodsInHierarchyRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' removeMethods: '.
	selectors asArray storeOn: aStream.
	aStream nextPutAll: ' from: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
