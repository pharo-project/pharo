"
This condition checks in the class and its superclasses if a class defines a given selector
"
Class {
	#name : 'ReUpToRootDefinesMethod',
	#superclass : 'RBClassCondition',
	#instVars : [
		'selector'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReUpToRootDefinesMethod >> check [

	^ self upToRootDefinesMethod 
]

{ #category : 'checking' }
ReUpToRootDefinesMethod >> doesClass: argClass definesMethod: aSelector [
	"We could avoid the aSelector argument. I do not have a good name."
	
	(argClass directlyDefinesMethod: aSelector) ifTrue: [ violators add: argClass. ^ true].
	^ argClass superclass isNotNil and: [ self doesClass: argClass superclass definesMethod: aSelector ]
]

{ #category : 'accessing' }
ReUpToRootDefinesMethod >> selector [

	^ selector
]

{ #category : 'accessing' }
ReUpToRootDefinesMethod >> selector: aSymbol [

	selector := aSymbol
]

{ #category : 'checking' }
ReUpToRootDefinesMethod >> upToRootDefinesMethod [

	(self doesClass: aClass definesMethod: selector) ifTrue: [ ^ true ].
	^ false
]
