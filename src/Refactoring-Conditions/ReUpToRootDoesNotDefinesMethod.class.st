Class {
	#name : 'ReUpToRootDoesNotDefinesMethod',
	#superclass : 'ReClassesCondition',
	#instVars : [
		'selector'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'as yet unclassified' }
ReUpToRootDoesNotDefinesMethod class >> classes: classes selector: selector [

	^ self new
		classes: classes;
		selector: selector
]

{ #category : 'instance creation' }
ReUpToRootDoesNotDefinesMethod >> class: aRBClass [ 

	classes := { aRBClass }.
	subjects := classes
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
		
	aStream
    nextPutAll: 'This name conflicts with an inherited method and may change program behavior.'

]

{ #category : 'accessing' }
ReUpToRootDoesNotDefinesMethod >> violators [

	^ violators ifNil: [
		  violators := (classes select: [ :class |
			  self doesClass: class definesMethod: selector ]) asOrderedCollection ]
]
