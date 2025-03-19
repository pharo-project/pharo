"
I represent a relative path (a position starting from Path workingDirectory)
"
Class {
	#name : 'RelativePath',
	#superclass : 'Path',
	#type : 'variable',
	#instVars : [
		'expandedParent'
	],
	#category : 'FileSystem-Path-Base',
	#package : 'FileSystem-Path',
	#tag : 'Base'
}

{ #category : 'converting' }
RelativePath >> asAbsolute [

	"Return absolute path to the receiver using workingDirectory path"

	"(../../plonk/nurp) asAbsolutePath -  where working directory
	 is (Path/program/test/griffle ).
	 returns 'Path/program/plonk/nurp')"

	| relativePathCollection countParents parentPath relativePathWithoutParent |

	relativePathCollection := self segments.
	countParents := self numberOfParentDirectories:
		                relativePathCollection.
	parentPath := self
		              parentPath: FileSystem workingDirectory pathString
		              numberOfDirectoriesUp: countParents.
	relativePathWithoutParent := relativePathCollection reject: [
		                             :directory |
		                             directory = self parentPathString ].

	^ (parentPath , self delimiter asString
	   , (self relativePathStringFrom: relativePathWithoutParent))
		  asFileReference
]

{ #category : 'private' }
RelativePath >> combinationsOfExpandedParentWithRelativePath: allDirectoriesInPath [
	"Returns a collection of parent paths combined with relative path"

	| paths readWriteStream |
	paths := OrderedCollection new.
	readWriteStream := ReadWriteStream with: String new.

	paths := allDirectoriesInPath collect: [ :directory |
		         paths isEmpty
			         ifTrue: [
				         readWriteStream
					         nextPutAll: Path root fullName;
					         nextPutAll: directory;
					         reset ]
			         ifFalse: [
				         readWriteStream
					         nextPutAll: paths last pathString;
					         nextPutAll: self delimiter asString;
					         nextPutAll: directory;
					         reset ].
		         Path from: readWriteStream upToEnd ].
	paths addFirst: Path root.

	^ paths
]

{ #category : 'accessing' }
RelativePath >> emptyPathString [
	"Answer the string representing an empty (size = 0) instance of the receiver.
	For a relative path, this is the current directory"

	^'.'
]

{ #category : 'navigating' }
RelativePath >> expandedParent [
  "Every relative path has a parent with absolute path"
	^ expandedParent ifNil: [ FileSystem workingDirectory ]

]

{ #category : 'navigating' }
RelativePath >> expandedParent: anAbsolutePath [
   "Parent's absolute path for the relative path"
   expandedParent := anAbsolutePath.
]

{ #category : 'private' }
RelativePath >> expandedParentAbsolutePath: aParentPathcollection [
	"Recreate absolute path without the jumped directories"

	| writeStream |
	writeStream := WriteStream with: String new.

	aParentPathcollection do: [ :directory |
		writeStream isEmpty
			ifTrue: [
				writeStream
					nextPutAll: Path root fullName;
					nextPutAll: directory ]
			ifFalse: [
				writeStream
					nextPutAll: self delimiter asString;
					nextPutAll: directory ] ].

	^ writeStream contents
]

{ #category : 'testing' }
RelativePath >> isAbsolute [
	^ false
]

{ #category : 'testing' }
RelativePath >> isRoot [
	^ false
]

{ #category : 'navigating' }
RelativePath >> numberOfParentDirectories: splitPathIntoSegments [
	"Returns a number of parent directories to travel up"

	^ splitPathIntoSegments occurrencesOf: self parentPathString
]

{ #category : 'navigating' }
RelativePath >> parentPath: workingAbsolutePathString numberOfDirectoriesUp: anInteger [
	"Processes how far in the parent directory tree to go"

	| delimiter parentDirectoryCollection |
	delimiter := self delimiter asString.
	parentDirectoryCollection := (workingAbsolutePathString substrings: delimiter) asOrderedCollection.
	self assert: parentDirectoryCollection size > anInteger.
	anInteger timesRepeat: [ parentDirectoryCollection removeLast ].

	^ self expandedParentAbsolutePath: parentDirectoryCollection
]

{ #category : 'accessing' }
RelativePath >> parentPathString [
	"Answer the string representing a parent instance of the receiver.
	For a relative path, this is the parent of current directory"

	^'..'
]

{ #category : 'printing' }
RelativePath >> printOn: aStream [

	aStream nextPutAll: 'Path '.
	self isWorkingDirectory
		ifTrue: [ aStream nextPutAll: 'workingDirectory' ]
		ifFalse:
			[ aStream
				nextPutAll: '* ''';
				nextPutAll: (self at: 1);
				nextPut: $'.
			2 to: self size do:
				[:i |
				aStream
					nextPutAll: ' / ''';
					nextPutAll: (self at: i);
					nextPut: $' ]]
]

{ #category : 'printing' }
RelativePath >> printOn: aStream delimiter: aCharacter [
	self isWorkingDirectory ifTrue: [aStream nextPut: $.. ^ self].
	super printOn: aStream delimiter: aCharacter
]

{ #category : 'private' }
RelativePath >> relativePathStringFrom: aRelativePathcollection [
	"Returns relative path"

	| writeStream |
	writeStream := WriteStream with: String new.

	aRelativePathcollection do: [ :directory |
		writeStream isEmpty
			ifTrue: [ writeStream nextPutAll: directory ]
			ifFalse: [
				writeStream
					nextPutAll: self delimiter asString;
					nextPutAll: directory ] ].

	^ writeStream contents
]

{ #category : 'enumerating' }
RelativePath >> withExpandedParent [
  "Returns collection of absolute parent path with relative path"
	| relativeWithAbsoluteParentPath allDirectoriesInPath |
	
	relativeWithAbsoluteParentPath := self asAbsolute.
	allDirectoriesInPath := (relativeWithAbsoluteParentPath pathString
		                         substrings: self delimiter asString)
		                        asOrderedCollection.
		
	^ self combinationsOfExpandedParentWithRelativePath: allDirectoriesInPath 
]
