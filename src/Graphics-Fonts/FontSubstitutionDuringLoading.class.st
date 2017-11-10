"
signaled by font loading code when reading a DiskProxy that calls for a missing font.
"
Class {
	#name : #FontSubstitutionDuringLoading,
	#superclass : #Notification,
	#instVars : [
		'familyName',
		'pixelSize'
	],
	#category : #Graphics-Fonts
}

{ #category : #'instance creation' }
FontSubstitutionDuringLoading class >> forFamilyName: aName pixelSize: aSize [
	^(self new)
		familyName: aName;
		pixelSize: aSize;
		yourself.
]

{ #category : #accessing }
FontSubstitutionDuringLoading >> defaultAction [
	familyName ifNil: [ familyName := 'NoName' ].
	pixelSize ifNil: [ pixelSize := 12 ].

	^((familyName beginsWith: 'Comic')
		ifTrue: [ TextStyle named: (StandardFonts buttonFont familyName) ]
		ifFalse: [ TextStyle default ]) fontOfSize: pixelSize.
]

{ #category : #accessing }
FontSubstitutionDuringLoading >> familyName [
	"Answer the value of familyName"

	^ familyName
]

{ #category : #accessing }
FontSubstitutionDuringLoading >> familyName: anObject [
	"Set the value of familyName"

	familyName := anObject
]

{ #category : #accessing }
FontSubstitutionDuringLoading >> pixelSize [
	"Answer the value of pixelSize"

	^ pixelSize
]

{ #category : #accessing }
FontSubstitutionDuringLoading >> pixelSize: anObject [
	"Set the value of pixelSize"

	pixelSize := anObject
]

{ #category : #printing }
FontSubstitutionDuringLoading >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(;
		nextPutAll: familyName;
		nextPut: $-;
		print: pixelSize;
		nextPut: $).
]
