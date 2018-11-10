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
	#name : #ClyTaggedClasses,
	#superclass : #ClyClassQuery,
	#instVars : [
		'tag'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance creation' }
ClyTaggedClasses class >> by: tagName [

	^self new 
		tag: tagName
]

{ #category : #'instance creation' }
ClyTaggedClasses class >> by: tagName from: aScopeClass [

	^(self from: aScopeClass)
		tag: tagName
]

{ #category : #comparing }
ClyTaggedClasses >> = anObject [
	"Answer whether the receiver and anObject represent the same object."
	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ tag = anObject tag
]

{ #category : #printing }
ClyTaggedClasses >> description [

	^'classes tagged with ', tag
]

{ #category : #comparing }
ClyTaggedClasses >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: tag hash
]

{ #category : #printing }
ClyTaggedClasses >> printExtraInfoOn: aStream [
	super printExtraInfoOn: aStream.

	^aStream nextPutAll: tag
]

{ #category : #testing }
ClyTaggedClasses >> selectsClass: aClass [
	^aClass isTaggedWith: tag
]

{ #category : #accessing }
ClyTaggedClasses >> tag [
	^ tag
]

{ #category : #accessing }
ClyTaggedClasses >> tag: anObject [
	tag := anObject
]
