"
I represent an absolute path (a position starting from Path root)
"
Class {
	#name : #AbsolutePath,
	#superclass : #Path,
	#type : #variable,
	#category : #'FileSystem-Path-Base'
}

{ #category : #private }
AbsolutePath class >> addEmptyElementTo: result [
]

{ #category : #'instance creation' }
AbsolutePath class >> from: aString delimiter: aDelimiterCharater [
	aString = '/'
		ifTrue: [ ^ self root ].
		
	^ super from: aString delimiter: aDelimiterCharater
]

{ #category : #testing }
AbsolutePath >> isAbsolute [
	^ true
]

{ #category : #testing }
AbsolutePath >> isRoot [
	^ self size = 0
]

{ #category : #printing }
AbsolutePath >> printOn: aStream [
	aStream nextPutAll: 'Path'.
	self isRoot
		ifTrue: [aStream nextPutAll: ' root']
		ifFalse:
			[1 to: self size do:
				[:i |
				aStream 
					nextPutAll: ' / ''';
				 	nextPutAll: (self at: i);
					nextPut: $']]
]

{ #category : #printing }
AbsolutePath >> printPathOn: aStream delimiter: aCharacter [
	"Print the path elements of the receiver, without the 'Path *' part"

	aStream nextPut: aCharacter.
	super printPathOn: aStream delimiter: aCharacter

]

{ #category : #enumerating }
AbsolutePath >> withParents [
	| root |
	root := Path root.
	^ super withParents
		remove: root ifAbsent: [ "In case it's absent we do not care." ];
		addFirst: root;
		yourself
]
