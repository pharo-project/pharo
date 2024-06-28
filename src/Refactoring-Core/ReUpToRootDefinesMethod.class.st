"
This condition checks in the class and its superclasses if a class defines a given selector
"
Class {
	#name : 'ReUpToRootDefinesMethod',
	#superclass : 'ReClassesCondition',
	#instVars : [
		'selector'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReUpToRootDefinesMethod >> class: aRBClass [ 

	classes := { aRBClass }
]

{ #category : 'checking' }
ReUpToRootDefinesMethod >> doesClass: argClass definesMethod: aSelector [
	"We could avoid the aSelector argument. I do not have a good name."

	(argClass directlyDefinesMethod: aSelector) ifTrue: [ ^ true ].
	^ argClass superclass isNotNil and: [ self doesClass: argClass superclass definesMethod: aSelector ]
]

{ #category : 'accessing' }
ReUpToRootDefinesMethod >> selector: aSymbol [

	selector := aSymbol
]

{ #category : 'displaying' }
ReUpToRootDefinesMethod >> violationMessageOn: aStream [

	self violators size = 1
		ifTrue: [ aStream nextPutAll: self violators first name; nextPutAll: ' does not define the method ' ]
		ifFalse: [ 
	self violators 
			do: [ :each | aStream nextPutAll: each name ] 
			separatedBy: [ aStream nextPutAll: ', '].
	aStream nextPutAll: ' don''t define the method '. ].
	aStream nextPutAll: selector.
]

{ #category : 'accessing' }
ReUpToRootDefinesMethod >> violators [

	^ violators ifNil: [
		  violators := (classes reject: [ :class |
			  self doesClass: class definesMethod: selector ]) asOrderedCollection ]
]
