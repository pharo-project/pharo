"
I am used to mark objects with icon.
To instantiate me execute:
	ClyIconProperty named: aSymbol 
 
Internal Representation and Key Implementation Points.

    Instance Variables
	iconName:		<Symbol>
"
Class {
	#name : #ClyIconProperty,
	#superclass : #ClyProperty,
	#instVars : [
		'iconName'
	],
	#category : #'Calypso-NavigationModel-Property'
}

{ #category : #'instance creation' }
ClyIconProperty class >> named: aString [
	^self new
		iconName: aString
]

{ #category : #accessing }
ClyIconProperty >> iconName [
	^ iconName
]

{ #category : #accessing }
ClyIconProperty >> iconName: anObject [
	iconName := anObject
]

{ #category : #printing }
ClyIconProperty >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: iconName.
	aStream nextPut: $).
]
