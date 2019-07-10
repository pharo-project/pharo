"
I represent class comment.

To create my instances use following script:
 
	ClyClassComment of: Object.
	
My instances are returned from class comments query.

Internal Representation and Key Implementation Points.

    Instance Variables
	definingClass:		<Class>
"
Class {
	#name : #ClyClassComment,
	#superclass : #Object,
	#instVars : [
		'definingClass'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #testing }
ClyClassComment class >> checkCalypsoItem: anObject isSameAs: anObject2 [
	^self checkCalypsoItem: anObject isEqualTo: anObject2
]

{ #category : #'item convertation' }
ClyClassComment class >> convertToCalypsoBrowserItem: aClassComment [

	| item |
	item := ClyBrowserItem named: 'Comment' with: aClassComment.
	item addProperty: (ClyClassDefinitionProperty of: aClassComment definingClass).
	^item
]

{ #category : #sorting }
ClyClassComment class >> defaultSortFunctionForCalypso [
	^ClySortByDefiningClassFunction ascending
]

{ #category : #testing }
ClyClassComment class >> match: aClassComment forQueryBrowserFilter: aQueryBrowserFilter [
	^aClassComment definingClass matchesQueryBrowserFilter: aQueryBrowserFilter
]

{ #category : #'instance creation' }
ClyClassComment class >> of: aClass [
	^self new 
		definingClass: aClass
]

{ #category : #printing }
ClyClassComment class >> printSystemPathTo: aClassComment [
	^aClassComment definingClass package name, ' / ' , aClassComment definingClass name
]

{ #category : #comparing }
ClyClassComment >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ definingClass = anObject definingClass
]

{ #category : #accessing }
ClyClassComment >> definingClass [
	^ definingClass
]

{ #category : #accessing }
ClyClassComment >> definingClass: anObject [
	definingClass := anObject
]

{ #category : #comparing }
ClyClassComment >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ definingClass hash
]

{ #category : #printing }
ClyClassComment >> printOn: aStream [
	super printOn: aStream.
	
	aStream nextPut: $(.
	definingClass printOn: aStream.
	aStream nextPut: $).
]
