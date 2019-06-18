"
I return methods which are marked by particular tag.

To instantiate my instances use following expressions: 

	ClyTaggedMethods by: #accessing.
	ClyTaggedMethods by: #accessing from: aScope
 
Internal Representation and Key Implementation Points.

    Instance Variables
	tag:		<Symbol>
"
Class {
	#name : #ClyTaggedMethodsQuery,
	#superclass : #ClyMethodQuery,
	#instVars : [
		'tag'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance creation' }
ClyTaggedMethodsQuery class >> by: tagName [

	^self new 
		tag: tagName
]

{ #category : #'instance creation' }
ClyTaggedMethodsQuery class >> by: tagName from: aScope [

	^(self from: aScope)
		tag: tagName
]

{ #category : #comparing }
ClyTaggedMethodsQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^tag = anObject tag
]

{ #category : #printing }
ClyTaggedMethodsQuery >> description [

	^'methods tagged with ', tag
]

{ #category : #comparing }
ClyTaggedMethodsQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ super hash bitXor: tag hash
]

{ #category : #'system changes' }
ClyTaggedMethodsQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	^(aSystemAnnouncement affectsMethodsTaggedWith: tag)
		and: [ scope includesMethodsAffectedBy: aSystemAnnouncement]
]

{ #category : #printing }
ClyTaggedMethodsQuery >> printExtraInfoOn: aStream [
	super printExtraInfoOn: aStream.

	^aStream nextPutAll: tag
]

{ #category : #testing }
ClyTaggedMethodsQuery >> selectsMethod: aMethod [
	^aMethod isTaggedWith: tag
]

{ #category : #accessing }
ClyTaggedMethodsQuery >> tag [
	^ tag
]

{ #category : #accessing }
ClyTaggedMethodsQuery >> tag: anObject [
	tag := anObject
]
