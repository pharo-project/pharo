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
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'selector'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #removing }
RBRemoveHierarchyMethodRefactoring class >> removeMethod: aSelector from: aClass [
	^ self new
		removeMethod: aSelector from: aClass
]

{ #category : #transforming }
RBRemoveHierarchyMethodRefactoring >> delete: aSelector in: aClass [
	 (aClass realClass includesSelector: aSelector) ifTrue: [
		 [self generateChangesFor: (RBRemoveMethodRefactoring
									model: self model
									removeMethod: aSelector
									from: aClass) ]
		on: RBRefactoringWarning
			do: [ :ex | ( self confirm:
				(ex messageText copyReplaceAll: 'Browse references?' with: 'Remove anyway?') )
				ifTrue:[ ex resume ] ] ]
]

{ #category : #transforming }
RBRemoveHierarchyMethodRefactoring >> deleteSelectorInSubclasses [
	class withAllSubclasses do:
		[ :each | each implements: selector ]
]

{ #category : #transforming }
RBRemoveHierarchyMethodRefactoring >> deleteSelectors [

	class withAllSubclasses do: [ :aClass |
		self delete: selector in: aClass ]
]

{ #category : #transforming }
RBRemoveHierarchyMethodRefactoring >> preconditions [

	^ RBCondition definesSelector: selector in: class
]

{ #category : #transforming }
RBRemoveHierarchyMethodRefactoring >> privateTransform [
	self deleteSelectors
]

{ #category : #removing }
RBRemoveHierarchyMethodRefactoring >> removeMethod: aSelector from: aClass [
	class := self classObjectFor: aClass.
	selector := aSelector
]

{ #category : #printing }
RBRemoveHierarchyMethodRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' removeMethods: '.
	selector storeOn: aStream.
	aStream nextPutAll: ' from: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
