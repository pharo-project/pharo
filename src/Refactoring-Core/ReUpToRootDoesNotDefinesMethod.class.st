"
This condition checks in the class and its superclasses if a class defines a given selector
"
Class {
	#name : 'ReUpToRootDoesNotDefinesMethod',
	#superclass : 'ReClassesCondition',
	#instVars : [
		'selector'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReUpToRootDoesNotDefinesMethod >> class: aRBClass [ 

	classes := { aRBClass }
]

{ #category : 'checking' }
ReUpToRootDoesNotDefinesMethod >> doesClass: argClass definesMethod: aSelector [
	"We could avoid the aSelector argument. I do not have a good name."

	(argClass directlyDefinesMethod: aSelector) ifTrue: [ ^ true ].
	^ argClass superclass isNotNil and: [ self doesClass: argClass superclass definesMethod: aSelector ]
]

{ #category : 'accessing' }
ReUpToRootDoesNotDefinesMethod >> selector: aSymbol [

	selector := aSymbol
]

{ #category : 'displaying' }
ReUpToRootDoesNotDefinesMethod >> violationMessageOn: aStream [

	self violators size = 1
		ifTrue: [ aStream nextPutAll: self violators first name; nextPutAll: ' defines the method ' ]
		ifFalse: [ 
	self violators 
			do: [ :each | aStream nextPutAll: each name ] 
			separatedBy: [ aStream nextPutAll: ', '].
	aStream nextPutAll: ' define the method '. ].
	aStream nextPutAll: selector.
]

{ #category : 'accessing' }
ReUpToRootDoesNotDefinesMethod >> violators [

	^ violators ifNil: [
		  violators := (classes select: [ :class |
			  self doesClass: class definesMethod: selector ]) asOrderedCollection ]
]
