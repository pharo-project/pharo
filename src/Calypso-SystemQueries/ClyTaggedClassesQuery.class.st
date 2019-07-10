"
I return all classes marked with particular tag.

To create my instance use following methods: 

	ClyTaggedClasses by: #SomeTag
	ClyTaggedClasses by: #SomeTag from: aScope
 
Internal Representation and Key Implementation Points.

    Instance Variables
	tag:		<Symbol>
"
Class {
	#name : #ClyTaggedClassesQuery,
	#superclass : #ClyClassQuery,
	#instVars : [
		'tag'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance creation' }
ClyTaggedClassesQuery class >> by: tagName [

	^self new 
		tag: tagName
]

{ #category : #'instance creation' }
ClyTaggedClassesQuery class >> by: tagName from: aScopeClass [

	^(self from: aScopeClass)
		tag: tagName
]

{ #category : #comparing }
ClyTaggedClassesQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."
	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ tag = anObject tag
]

{ #category : #printing }
ClyTaggedClassesQuery >> description [

	^'classes tagged with ', tag
]

{ #category : #comparing }
ClyTaggedClassesQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: tag hash
]

{ #category : #printing }
ClyTaggedClassesQuery >> printExtraInfoOn: aStream [
	super printExtraInfoOn: aStream.

	^aStream nextPutAll: tag
]

{ #category : #testing }
ClyTaggedClassesQuery >> selectsClass: aClass [
	^aClass isTaggedWith: tag
]

{ #category : #accessing }
ClyTaggedClassesQuery >> tag [
	^ tag
]

{ #category : #accessing }
ClyTaggedClassesQuery >> tag: anObject [
	tag := anObject
]
