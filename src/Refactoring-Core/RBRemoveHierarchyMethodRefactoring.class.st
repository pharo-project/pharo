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
		'selectors'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #removing }
RBRemoveHierarchyMethodRefactoring class >> removeMethods: aSelectorsCollection from: aClass [ 
	
	^ self new removeMethods: aSelectorsCollection from: aClass
]

{ #category : #transforming }
RBRemoveHierarchyMethodRefactoring >> delete: aSelectorsCollection in: aClass [
	
	| containingMethods |
	containingMethods := aSelectorsCollection select: [ :sel | aClass realClass includesSelector: sel ].
	
	containingMethods ifNotEmpty: [
		 [self generateChangesFor: (RBRemoveMethodsRefactoring
									model: self model
									methods: containingMethods 
									from: aClass) ]
		on: RBRefactoringWarning
			do: [ :ex | ( self confirm:
				(ex messageText copyReplaceAll: 'Browse references?' with: 'Remove anyway?') )
				ifTrue:[ ex resume ] ] ]
]

{ #category : #transforming }
RBRemoveHierarchyMethodRefactoring >> deleteSelectors [

	class withAllSubclasses do: [ :aClass |
		self delete: selectors in: aClass ]
]

{ #category : #preconditions }
RBRemoveHierarchyMethodRefactoring >> preconditions [

	^ selectors inject: self trueCondition into: [ :cond :selector |
		  cond & (RBCondition definesSelector: selector in: class) ]
]

{ #category : #transforming }
RBRemoveHierarchyMethodRefactoring >> privateTransform [
	self deleteSelectors
]

{ #category : #removing }
RBRemoveHierarchyMethodRefactoring >> removeMethods: aSelectorsCollection from: aClass [

	class := self classObjectFor: aClass.
	selectors := aSelectorsCollection
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
