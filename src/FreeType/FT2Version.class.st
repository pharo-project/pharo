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
	"
	FT2Version current
	"
	^ [(self new)
		libraryVersion;
		yourself] on: Error do: [:ex | ex return: nil]
]

{ #category : #primitives }
FT2Version >> libraryVersion [
	<primitive: 'primitiveVersion' module: 'FT2Plugin'>
	^self primitiveFailed.

]

{ #category : #accessing }
FT2Version >> major [
	^major
]

{ #category : #accessing }
FT2Version >> minor [
	^minor
]

{ #category : #accessing }
FT2Version >> patch [
	^patch
]

{ #category : #printing }
FT2Version >> printOn: aStream [
	aStream print: major; nextPut: $.; print: minor; nextPut:$.; print: patch.
]
