"
This condition checks in the class and its superclasses and subclasses if a class defines a given selector.
It could be a subclass of `REUpToRootDefinesMethod` since it extends the search.
"
Class {
	#name : 'REHierarchyDefinesMethod',
	#superclass : 'RBClassCondition',
	#instVars : [
		'selector'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
REHierarchyDefinesMethod >> check [

	^ self hierarchyDefinesMethod
]

{ #category : 'checking' }
REHierarchyDefinesMethod >> doesClass: argClass definesMethod: aSelector [
	"We could avoid the aSelector argument. I do not have a good name."
	
	(argClass directlyDefinesMethod: aSelector) ifTrue: [ violators add: argClass. ^ true].
	^ argClass superclass isNotNil and: [ self doesClass: argClass superclass definesMethod: aSelector ]
]

{ #category : 'as yet unclassified' }
REHierarchyDefinesMethod >> hierarchyDefinesMethod [

	(self doesClass: aClass definesMethod: selector) ifTrue: [ ^ true ].
	^ self subclassOf: aClass redefines: selector
]

{ #category : 'accessing' }
REHierarchyDefinesMethod >> selector [

	^ selector
]

{ #category : 'accessing' }
REHierarchyDefinesMethod >> selector: aSymbol [

	selector := aSymbol
]

{ #category : 'as yet unclassified' }
REHierarchyDefinesMethod >> subclassOf: argClass redefines: aSelector [

	| defs |
	defs := argClass allSubclasses select: [ :each | (each directlyDefinesMethod: aSelector) ].
	defs ifNotEmpty: [ violators add: defs. ^ true ].
	^ false.
]
