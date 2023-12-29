"
This condition checks in the class and its superclasses if a class defines a given selector
"
Class {
	#name : 'REUpToRootDefinesMethod',
	#superclass : 'RBClassCondition',
	#instVars : [
		'selector'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
REUpToRootDefinesMethod >> check [

	^ self upToRootDefinesMethod 
]

{ #category : 'checking' }
REUpToRootDefinesMethod >> doesClass: argClass definesMethod: aSelector [
	"We could avoid the aSelector argument. I do not have a good name."
	
	(argClass directlyDefinesMethod: aSelector) ifTrue: [ violators add: argClass. ^ true].
	^ argClass superclass isNotNil and: [ self doesClass: argClass superclass definesMethod: aSelector ]
]

{ #category : 'accessing' }
REUpToRootDefinesMethod >> selector [

	^ selector
]

{ #category : 'accessing' }
REUpToRootDefinesMethod >> selector: aSymbol [

	selector := aSymbol
]

{ #category : 'checking' }
REUpToRootDefinesMethod >> upToRootDefinesMethod [

	(self doesClass: aClass definesMethod: selector) ifTrue: [ ^ true ].
	^ false
]
