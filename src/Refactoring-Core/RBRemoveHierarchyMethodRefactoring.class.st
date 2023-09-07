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
	#name : #RBRemoveHierarchyMethodRefactoring,
	#superclass : #RBCompositeRefactoring,
	#instVars : [
		'selectors',
		'class'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #removing }
RBRemoveHierarchyMethodRefactoring class >> selectors: aSelectorsCollection from: aClass [ 
	
	^ self new selectors: aSelectorsCollection from: aClass
]

{ #category : #preconditions }
RBRemoveHierarchyMethodRefactoring >> applicabilityPreconditions [ 

	^ selectors
			inject: self trueCondition
			into: [ :cond :sel | cond & (RBCondition definesSelector: sel in: class) ]
]

{ #category : #transforming }
RBRemoveHierarchyMethodRefactoring >> delete: aSelectorsCollection in: aClass [

	| containingMethods |
	containingMethods := aSelectorsCollection select: [ :sel |
		                     aClass realClass includesSelector: sel ].
	containingMethods ifNotEmpty: [
		refactorings add: (RBRemoveMethodsRefactoring
				 model: self model
				 selectors: containingMethods
				 from: aClass) ]
]

{ #category : #transforming }
RBRemoveHierarchyMethodRefactoring >> initializeRefactorings [

	class withAllSubclasses do: [ :aClass |
		self delete: selectors in: aClass ]
]

{ #category : #removing }
RBRemoveHierarchyMethodRefactoring >> selectors: aSelectorsCollection from: aClass [

	class := self classObjectFor: aClass.
	selectors := aSelectorsCollection.
	refactorings := OrderedCollection new.
	self initializeRefactorings 
]

{ #category : #printing }
RBRemoveHierarchyMethodRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' removeMethods: '.
	selectors asArray storeOn: aStream.
	aStream nextPutAll: ' from: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
