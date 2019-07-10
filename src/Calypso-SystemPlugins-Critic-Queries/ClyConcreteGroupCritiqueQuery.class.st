"
I return critiques of my base query which belongs to particular group.

To create my instance use following expression: 

	ClyConcreteGroupCritiques filter: aCritiqueQuery from: aScope byGroup: aString
 
Internal Representation and Key Implementation Points.

    Instance Variables
	groupName:		<String>
"
Class {
	#name : #ClyConcreteGroupCritiqueQuery,
	#superclass : #ClyFilteringCritiqueQuery,
	#instVars : [
		'groupName'
	],
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #'instance creation' }
ClyConcreteGroupCritiqueQuery class >> filter: aCritiqueQuery from: aScope byGroup: aString [

	^(self filter: aCritiqueQuery from: aScope)
		groupName: aString
]

{ #category : #comparing }
ClyConcreteGroupCritiqueQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
		
	^groupName = anObject groupName 
]

{ #category : #printing }
ClyConcreteGroupCritiqueQuery >> description [

	^groupName
]

{ #category : #accessing }
ClyConcreteGroupCritiqueQuery >> groupName [
	^ groupName
]

{ #category : #accessing }
ClyConcreteGroupCritiqueQuery >> groupName: anObject [
	groupName := anObject
]

{ #category : #comparing }
ClyConcreteGroupCritiqueQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: groupName hash
]

{ #category : #printing }
ClyConcreteGroupCritiqueQuery >> printExtraInfoOn: aStream [
	super printExtraInfoOn: aStream.

	aStream nextPutAll: groupName
]

{ #category : #testing }
ClyConcreteGroupCritiqueQuery >> selectsCritique: aCritique [
	^aCritique rule group = groupName
]
