"
Do not rearrange these fields!

This is used to report FT2 version information. Its fields must remain unchanged, or you must change FT2Plugin>>primitiveVersion.
"
Class {
	#name : #FT2Version,
	#superclass : #Object,
	#instVars : [
		'major',
		'minor',
		'patch'
	],
	#category : #'FreeType-Base'
}

{ #category : #'instance creation' }
FT2Version class >> current [
	
	^ FT2Library current libraryVersion
]

{ #category : #accessing }
FT2Version >> major [
	^major
]

{ #category : #accessing }
FT2Version >> major: anObject [
	major := anObject
]

{ #category : #accessing }
FT2Version >> minor [
	^minor
]

{ #category : #accessing }
FT2Version >> minor: anObject [
	minor := anObject
]

{ #category : #accessing }
FT2Version >> patch [
	^patch
]

{ #category : #accessing }
FT2Version >> patch: anObject [
	patch := anObject
]

{ #category : #printing }
FT2Version >> printOn: aStream [
	aStream print: major; nextPut: $.; print: minor; nextPut:$.; print: patch.
]
