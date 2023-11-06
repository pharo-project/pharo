"
I represent an absolute path (a position starting from Path root)
"
Class {
	#name : 'AbsolutePath',
	#superclass : 'Path',
	#type : 'variable',
	#category : 'FileSystem-Path-Base',
	#package : 'FileSystem-Path',
	#tag : 'Base'
}

{ #category : 'private' }
AbsolutePath class >> addEmptyElementTo: result [
]

{ #category : 'private' }
AbsolutePath class >> addParentElementTo: result on: filesystem [
	"The parent directory may be a symbolic link, in which case it must be resolved to its target path before determining the result."

	result ifNotEmpty:
		[ | parentReference entry |
		parentReference := filesystem referenceTo: (self withAll: result).
		entry := parentReference symlinkEntry.
		entry isSymlink ifTrue:
			[ result removeAll.
			result addAll: entry targetPath segments ] ].

	(result isEmpty or: [ result last = '..' ])
		ifTrue: [ result add: '..' ]
		ifFalse: [ result removeLast ]
]

{ #category : 'instance creation' }
AbsolutePath class >> from: aString delimiter: aDelimiterCharater [
	aString = '/'
		ifTrue: [ ^ self root ].

	^ super from: aString delimiter: aDelimiterCharater
]

{ #category : 'private' }
AbsolutePath class >> removeRedundantSegments: segments [
	"Remove redundant elements ('.' and '') from the supplied segments.
	The parent of the root directory is itself."
	| newSegments |

	newSegments := segments asOrderedCollection copy.
	[ newSegments isNotEmpty and: [ newSegments first = '..' ] ] whileTrue:
		[ newSegments removeFirst ].
	^ newSegments select:
		[ :segment | segment notEmpty and: [ segment ~= '.' ] ]
]

{ #category : 'testing' }
AbsolutePath >> isAbsolute [
	^ true
]

{ #category : 'testing' }
AbsolutePath >> isRoot [
	^ self size = 0
]

{ #category : 'printing' }
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

{ #category : 'printing' }
AbsolutePath >> printPathOn: aStream delimiter: aCharacter [
	"Print the path elements of the receiver, without the 'Path *' part"

	aStream nextPut: aCharacter.
	super printPathOn: aStream delimiter: aCharacter
]

{ #category : 'enumerating' }
AbsolutePath >> withParents [
	| root |
	root := Path root.
	^ super withParents
		remove: root ifAbsent: [ "In case it's absent we do not care." ];
		addFirst: root;
		yourself
]
