"
This condition checks in the class and its superclasses and subclasses if a class defines a given selector.
It could be a subclass of `REUpToRootDefinesMethod` since it extends the search.
"
Class {
	#name : 'ReHierarchyDefinesMethod',
	#superclass : 'ReClassCondition',
	#instVars : [
		'selector'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReHierarchyDefinesMethod >> check [

	^ self hierarchyDefinesMethod
]

{ #category : 'checking' }
ReHierarchyDefinesMethod >> doesClass: argClass definesMethod: aSelector [
	"We could avoid the aSelector argument. I do not have a good name."
	
	(argClass directlyDefinesMethod: aSelector) ifTrue: [ violators add: argClass. ^ true].
	^ argClass superclass isNotNil and: [ self doesClass: argClass superclass definesMethod: aSelector ]
]

{ #category : 'checking' }
ReHierarchyDefinesMethod >> hierarchyDefinesMethod [

	(self doesClass: class definesMethod: selector) ifTrue: [ ^ true ].
	^ self subclassOf: class redefines: selector
]

{ #category : 'accessing' }
ReHierarchyDefinesMethod >> selector [

	^ selector
]

{ #category : 'accessing' }
ReHierarchyDefinesMethod >> selector: aSymbol [

	selector := aSymbol
]

{ #category : 'checking' }
ReHierarchyDefinesMethod >> subclassOf: argClass redefines: aSelector [

	| defs |
	defs := argClass allSubclasses select: [ :each | (each directlyDefinesMethod: aSelector) ].
	defs ifNotEmpty: [ violators add: defs. ^ true ].
	^ false.
]
