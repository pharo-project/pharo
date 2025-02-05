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
	#name : 'ReRemoveMethodsInHierarchyRefactoring',
	#superclass : 'ReRemoveMethodsRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'transforming' }
ReRemoveMethodsInHierarchyRefactoring >> initializeRefactorings [

	classSelectorMapping keysAndValuesDo: [ :class :selectors |
		class allSubclasses do: [ :aClass |
			self remove: selectors in: aClass ] ]
]

{ #category : 'transforming' }
ReRemoveMethodsInHierarchyRefactoring >> remove: aSelectorsCollection in: aClass [

	| containingMethods |
	containingMethods := aSelectorsCollection select: [ :sel |
		                     aClass realClass includesSelector: sel ].
	containingMethods ifNotEmpty: [
		refactorings add: (ReRemoveMethodsRefactoring
				 model: self model
				 selectors: containingMethods
				 from: aClass) ]
]

{ #category : 'accessing' }
ReRemoveMethodsInHierarchyRefactoring >> selectors: aSelectorsCollection from: aClass [

	super selectors: aSelectorsCollection from: aClass.
	self initializeRefactorings 
]
