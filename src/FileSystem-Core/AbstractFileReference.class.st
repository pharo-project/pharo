"
I am an abstract superclass for FileLocator and FileReference. By implementing most of the methods on myself most code duplucation between the locator and the reference can be avoided
"
Class {
	#name : #AbstractFileReference,
	#superclass : #Object,
	#category : #'FileSystem-Core-Public'
}

{ #category : #copying }
AbstractFileReference >> , extension [
	^ self resolve, extension
]

{ #category : #navigating }
AbstractFileReference >> / aString [
	"aString is either a file or path.  If aString is relative, it is appended to the receiver, if it is absolute, an instance of the receiver with the path is answered"

	^ self withPath: (self path resolvePath: (self fileSystem pathFromString: aString))
]

{ #category : #comparing }
AbstractFileReference >> <= other [
	^ self path <= other path
]

{ #category : #accessing }
AbstractFileReference >> absolutePath [
	"Returns the absolute path"
	
	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractFileReference >> accessTime [
	^ self resolve accessTime 
]

{ #category : #enumerating }
AbstractFileReference >> allChildren [
	"Return all the files and folders recursively nested in the receiver"
	
	^ CollectVisitor breadthFirst: self resolve collect: [:entry | entry reference]
]

{ #category : #enumerating }
AbstractFileReference >> allChildrenMatching: aPattern [
    "Return all the files and folders recursively nested in the receiver and matching the pattern, aPattern"
    
    ^ self allChildren select: [ :each |  aPattern match: each basename ]

]

{ #category : #enumerating }
AbstractFileReference >> allDirectories [
	"Return all the directories recursively nested in the receiver."
	^ (SelectVisitor breadthFirst: self resolve select: [:entry | entry isDirectory ])
		collect: [ :each| each reference ]
]

{ #category : #enumerating }
AbstractFileReference >> allDirectoriesMatching: aPattern [
    "Return all the directories recursively nested in the receiver."
    ^ (SelectVisitor 
			breadthFirst: self resolve 
			select: [:entry | entry isDirectory and: [ aPattern match: entry basename ] ])
        collect: [ :each | each reference ]

]

{ #category : #enumerating }
AbstractFileReference >> allEntries [
	^ CollectVisitor breadthFirst: self resolve
]

{ #category : #enumerating }
AbstractFileReference >> allFiles [
	"Return all the files (not directories) recursively nested in the receiver."
	^ (SelectVisitor breadthFirst: self resolve select: [:entry | entry isFile ])
		collect: [ :each| each reference ]
]

{ #category : #converting }
AbstractFileReference >> asAbsolute [ 
	self subclassResponsibility
]

{ #category : #converting }
AbstractFileReference >> asFileReference [ 
	self subclassResponsibility 
]

{ #category : #converting }
AbstractFileReference >> asPath [
	"Answer the receiver's path"

	self subclassResponsibility 
]

{ #category : #delegated }
AbstractFileReference >> asPathWith: anObject [
	^ self resolve asPathWith: anObject
]

{ #category : #resolving }
AbstractFileReference >> asResolvedBy: anObject [
	^ anObject resolveReference: self
]

{ #category : #accessing }
AbstractFileReference >> base [
	"Returns the base of the basename, i.e. /foo/gloops.taz base is 'gloops'"
	^ self fullPath base
]

{ #category : #accessing }
AbstractFileReference >> basename [
	"Returns the basename, i.e. /foo/gloops.taz basename is 'gloops.taz'"
	^ self fullPath basename
]

{ #category : #accessing }
AbstractFileReference >> basenameWithIndicator [
	"Returns the basename with the indicator appended, i.e. /foo/gloops.taz basenameWithIndicator is 'gloops.taz', whereras /foo basenameWithIndicator is 'foo/'" 
	^ self basename, self indicator   
]

{ #category : #accessing }
AbstractFileReference >> basenameWithoutExtension [
	"Returns the basename, i.e. /foo/gloops.taz basenameWithoutExtension is 'gloops'"
	^ self fullPath basenameWithoutExtension
]

{ #category : #accessing }
AbstractFileReference >> basenameWithoutExtension: anExtension [
	"Returns the basename without specified extension (if any)
	('/foo/gloops.taz' asFileReference basenameWithoutExtension: 'taz') = 'gloops'
	"
	^ self fullPath basenameWithoutExtension: anExtension
]

{ #category : #streams }
AbstractFileReference >> binaryReadStream [
	^ self subclassResponsibility
]

{ #category : #streams }
AbstractFileReference >> binaryReadStreamDo: aBlock [
	| stream |
	stream := self binaryReadStream.
	^ [ aBlock value: stream ] 
		ensure: [ stream close ]
]

{ #category : #streams }
AbstractFileReference >> binaryReadStreamDo: doBlock ifAbsent: absentBlock [
	^ self isFile 
		ifTrue: [ self binaryReadStreamDo: doBlock ]
		ifFalse: absentBlock
]

{ #category : #streams }
AbstractFileReference >> binaryReadStreamIfAbsent: absentBlock [
	^ self isFile
		ifTrue: [ self binaryReadStream ]
		ifFalse: absentBlock
]

{ #category : #streams }
AbstractFileReference >> binaryWriteStream [
	"Answer a binary write stream on the receiver"

	^ self subclassResponsibility
]

{ #category : #streams }
AbstractFileReference >> binaryWriteStreamDo: aBlock [
	"Pass a binary write stream on the receiver to the supplied block, and ensure that the stream is closed after evaluation."

	| stream |

	stream := self binaryWriteStream.
	^ [ aBlock value: stream ] 
		ensure: [ stream close ]
]

{ #category : #streams }
AbstractFileReference >> binaryWriteStreamDo: doBlock ifPresent: presentBlock [
	^ self isFile
		ifTrue: presentBlock
		ifFalse: [ self binaryWriteStreamDo: doBlock ]
]

{ #category : #delegated }
AbstractFileReference >> canonicalize [
	"Answer the receiver with references to the current folder (.) and parent folder (..) removed"

	^ self withPath: self resolve path canonicalize
]

{ #category : #accessing }
AbstractFileReference >> changeTime [
	^ self resolve changeTime 
]

{ #category : #private }
AbstractFileReference >> childGeneratorBlock: doBlock matching: patterns [
	"
	FileSystem workingDirectory filesMatching: '*'
	FileSystem workingDirectory filesMatching: '*.image;*.changes'
	"
	
	| files reference|
	files := Set new.
	reference := self resolve.
	
	(patterns findTokens: ';', String crlf) do: [ :pattern | 
		doBlock value: reference value: [ :basename|
			(pattern match: basename)
				ifTrue: [ files add: (self / basename) ]]].
	
	^files asOrderedCollection
]

{ #category : #enumerating }
AbstractFileReference >> childNames [
	^ self children collect: #basename
]

{ #category : #enumerating }
AbstractFileReference >> children [
	"Answer an array containing references to the direct children of this reference."
	| reference |
	reference := self resolve.
	^ (reference fileSystem childNamesAt: reference path) 
		collect: [ :aName | self / aName ]
]

{ #category : #enumerating }
AbstractFileReference >> childrenMatching: patterns [
	"
	FileSystem disk workingDirectory childrenMatching: '*'
	FileSystem disk workingDirectory childrenMatching: '*.image;*.changes'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem childNamesAt: reference path do: aBlock ]
		matching:  patterns 
]

{ #category : #comparing }
AbstractFileReference >> contains: anObject [
	"Return true if anObject is in a subfolder of me"
	^ anObject isContainedBy: self resolve
]

{ #category : #comparing }
AbstractFileReference >> containsPath: aPath [
	^ self fullPath containsPath: aPath
]

{ #category : #accessing }
AbstractFileReference >> contents [
	self readStreamDo: [ :stream |
		^ stream contents ]
]

{ #category : #operations }
AbstractFileReference >> copyAllTo: aResolvable [
	"Performs a deep copy of the receiver, to a location specified by the argument. If the receiver is a file, the file will be copied; if a directory, the directory and its contents will be copied recursively. The argument must be a reference that doesn't exist; it will be created by the copy."
	
	CopyVisitor copy: self resolve asAbsolute to: aResolvable resolve
]

{ #category : #delegated }
AbstractFileReference >> copyTo: aReference [
	^ self resolve copyTo: aReference resolve
]

{ #category : #copying }
AbstractFileReference >> copyWithPath: newPath [
	self subclassResponsibility
]

{ #category : #enumerating }
AbstractFileReference >> createDirectory [
	"Verifies that the directory does not exist and only creates if necessary. Do not remove files contained if they exist.If the parents does not exist return an exception"
	self parent exists ifFalse:[DirectoryDoesNotExist signalWith: self parent path].
	^ self resolve ensureCreateDirectory
]

{ #category : #enumerating }
AbstractFileReference >> createFile [
	"Create if necessary a file for the receiver. If the parent does not exist return an exception"
	self parent exists ifFalse:[DirectoryDoesNotExist signalWith: self parent path].
	self writeStream close.
	
]

{ #category : #accessing }
AbstractFileReference >> creationTime [
	^ self resolve creationTime 
]

{ #category : #operations }
AbstractFileReference >> delete [
	"Delete the receiver, does raise an error if it is not present."
	
	^ self resolve delete
]

{ #category : #operations }
AbstractFileReference >> deleteAll [
	"Delete this directory and all children of it, raise an error if the file does not exist.
	Don't follow symbolic links (so files outside the receiver's tree will not be deleted)."
	DeleteVisitor delete: self resolve
]

{ #category : #operations }
AbstractFileReference >> deleteAllChildren [
	"Delete all children of the receiver, raise an error if the receiver does not exist.
	Don't follow symbolic links (so files outside the receiver's tree will not be deleted)."
	
	self children do: [:aReference | aReference deleteAll ]
]

{ #category : #operations }
AbstractFileReference >> deleteIfAbsent: aBlock [
	"Delete the receiver, when it does not exist evaluate the block"
	self resolve deleteIfAbsent: aBlock
]

{ #category : #testing }
AbstractFileReference >> deviceId [
	^ self resolve deviceId
]

{ #category : #enumerating }
AbstractFileReference >> directories [
	"Return all the directories (by opposition to files) contained in the receiver"

	| reference |
	reference := self resolve.
	^ (reference fileSystem directoryNamesAt: reference path) 
		collect: [ :aName | self withPath: self path / aName ]
]

{ #category : #enumerating }
AbstractFileReference >> directoriesMatching: patterns [
	"
	FileSystem disk workingDirectory directoriesMatching: '*'
	FileSystem disk workingDirectory directoriesMatching: 'package-cache'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem directoryNamesAt: reference path do: aBlock ]
		matching:  patterns 
]

{ #category : #enumerating }
AbstractFileReference >> directoryNames [
	^ self directories collect: #basename
]

{ #category : #operations }
AbstractFileReference >> ensureCreateDirectory [
	"Verifies that the directory does not exist and only creates if necessary. Do not remove files contained if they exist.Creates the parents if needed"
	^ self resolve ensureCreateDirectory
]

{ #category : #operations }
AbstractFileReference >> ensureCreateFile [
	"Create if necessary a file for the receiver. If the parent does not exist creates it"
	self parent ensureCreateDirectory.
	self writeStream close.
	
]

{ #category : #operations }
AbstractFileReference >> ensureDelete [
	"Delete the file and does not raise exception if it does not exist contrary to delete.
	However if it is a directory and it has children an error is signaled. If it is required to 
	delete even with children, use #ensureDeleteAll."
	
	self deleteIfAbsent: [].
]

{ #category : #operations }
AbstractFileReference >> ensureDeleteAll [
	"Delete this directory and all children of it, and does not raise an error if the file does not exist."
	
	self exists ifFalse: [ ^ self  ].
	self deleteAll
	
]

{ #category : #operations }
AbstractFileReference >> ensureDeleteAllChildren [
	"delete all children of the receiver and does not raise an error if the receiver does not exist"
	
	self exists ifFalse: [ ^ self  ].
	self deleteAllChildren
]

{ #category : #enumerating }
AbstractFileReference >> entries [
	^ self resolve entries
]

{ #category : #accessing }
AbstractFileReference >> entry [
	^ self resolve entry
]

{ #category : #delegated }
AbstractFileReference >> exists [
	"Return true if the file reference exist (e.g., if there is a file on the hard disk pointed by the file reference)
	
	E.g., (if you are on Unix or OSX)
	'/tmp/' asFileReference exists => true
	'/tmp/zorkbar' asFileReference exists => false
	"
	^ self resolve exists
]

{ #category : #accessing }
AbstractFileReference >> extension [
	
	^ self fullPath extension.
]

{ #category : #accessing }
AbstractFileReference >> extensions [
	  "#('foo' 'foo.tar' 'foo.tar.gz' 'foo.1.tar' 'foo.1.tar.gz') collect: [:thing| thing extensions] => #(#() #('tar') #('tar' 'gz') #('1' 'tar') #('1' 'tar' 'gz'))"
	
        ^ self fullPath extensions
]

{ #category : #ToDeprecate }
AbstractFileReference >> fileIn [

	self readStreamDo: [ :stream | 
		CodeImporter evaluateFileStream: stream ]
]

{ #category : #enumerating }
AbstractFileReference >> fileNames [
	^ self files collect: #basename
]

{ #category : #accessing }
AbstractFileReference >> fileSystem [
	^ self resolve fileSystem
]

{ #category : #enumerating }
AbstractFileReference >> files [
	"Return all the files (as opposed to folders) contained in the receiver"
	
	| reference |
	reference := self resolve.
	^ (reference fileSystem fileNamesAt: reference path) 
		collect: [ :aName | self withPath: self path / aName ]
]

{ #category : #enumerating }
AbstractFileReference >> filesMatching: patterns [
	"
	FileSystem disk workingDirectory filesMatching: '*'
	FileSystem disk workingDirectory filesMatching: '*.image;*.changes'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem fileNamesAt: reference path do: aBlock ]
		matching:  patterns 
]

{ #category : #delegated }
AbstractFileReference >> fullName [

	^ self resolve fullName
]

{ #category : #accessing }
AbstractFileReference >> fullNameWithIndicator [
	"Returns the basename with the indicator appended, i.e. /foo/gloops.taz basenameWithIndicator is '/foo/gloops.taz', whereras /foo basenameWithIndicator is '/foo/'" 
	^ self fullName, self indicator   
]

{ #category : #accessing }
AbstractFileReference >> fullPath [
	"Returns the absolute path, better use absolutePath"
	
	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractFileReference >> gid [
	^ self resolve gid
]

{ #category : #enumerating }
AbstractFileReference >> glob: aBlock [
	^ SelectVisitor breadthFirst: self resolve select: aBlock
]

{ #category : #testing }
AbstractFileReference >> hasChildren [
	^self resolve hasChildren
]

{ #category : #testing }
AbstractFileReference >> hasDirectories [
	^self resolve hasDirectories
]

{ #category : #testing }
AbstractFileReference >> hasFiles [
	^self resolve hasFiles
]

{ #category : #comparing }
AbstractFileReference >> hash [
	"Hash is reimplemented because #= is reimplemented"
	^ self path  hash
]

{ #category : #accessing }
AbstractFileReference >> humanReadableSize [
	^ self size humanReadableSIByteSize
]

{ #category : #testing }
AbstractFileReference >> ifFile: fBlock ifDirectory: dBlock ifAbsent: aBlock [
	^ self isFile
		ifTrue: fBlock
		ifFalse: [
			self isDirectory
				ifTrue: dBlock
				ifFalse: aBlock ]
]

{ #category : #printing }
AbstractFileReference >> indicator [
	"Returns a string indicating the type of reference:
	- '?' for a non existing reference',
	- '/' for a directory,
	- the empty string for a file."
	"When this framework gets more complete, it is possible to extend this behavior with the following indicators (taken from unix ls utility):
	- '*' for a regular file that is executable
	- '@' for a symbolic link
	- '|' for FIFOs
	- '=' for sockets
	- '>' for doors"
	^ self exists
		ifTrue: [self isDirectory ifTrue: ['/'] ifFalse: ['']  ]
		ifFalse: ['?']
]

{ #category : #accessing }
AbstractFileReference >> inode [
	^ self resolve inode
]

{ #category : #testing }
AbstractFileReference >> isAbsolute [ 
	self subclassResponsibility 
]

{ #category : #accessing }
AbstractFileReference >> isBlock [
	^ self resolve isBlock 
]

{ #category : #accessing }
AbstractFileReference >> isCharacter [
	^ self resolve isCharacter
]

{ #category : #testing }
AbstractFileReference >> isChildOf: anObject [
	^ self parent = anObject
]

{ #category : #comparing }
AbstractFileReference >> isContainedBy: anObject [
	"DoubleDispatch helper for #contains:"
	^ anObject containsReference: self resolve
]

{ #category : #testing }
AbstractFileReference >> isDirectory [
	^ self resolve isDirectory
]

{ #category : #testing }
AbstractFileReference >> isExecutable [
	^ self resolve isExecutable
]

{ #category : #testing }
AbstractFileReference >> isFIFO [
	^ self resolve isFIFO
]

{ #category : #testing }
AbstractFileReference >> isFile [
	^ self resolve isFile
]

{ #category : #testing }
AbstractFileReference >> isReadable [
	^ self resolve isReadable
]

{ #category : #testing }
AbstractFileReference >> isRelative [ 
	self subclassResponsibility 
]

{ #category : #testing }
AbstractFileReference >> isRoot [
	^ self resolve isRoot
]

{ #category : #testing }
AbstractFileReference >> isSocket [
	^ self resolve isSocket
]

{ #category : #testing }
AbstractFileReference >> isSymlink [
	^ self resolve isSymlink
]

{ #category : #testing }
AbstractFileReference >> isWritable [
	^ self resolve isWritable
]

{ #category : #accessing }
AbstractFileReference >> item [
	^ self
]

{ #category : #navigating }
AbstractFileReference >> makeRelative: anObject [
	^ anObject relativeToReference: self resolve
]

{ #category : #accessing }
AbstractFileReference >> modificationTime [
	^ self resolve modificationTime 
]

{ #category : #operations }
AbstractFileReference >> moveTo: aReference [
	"Move the receiver in the location passed as argument.
	
	(FileSystem disk workingDirectory / 'paf' ) ensureCreateFile.
	(FileSystem disk workingDirectory / 'fooFolder') ensureCreateDirectory. 
	(FileSystem disk workingDirectory / 'paf' ) moveTo: (FileSystem disk workingDirectory / 'fooFolder' / 'paf')
	
	Note that the receiver is modified to point to the new location."

	^ self resolve moveTo: aReference
]

{ #category : #utilities }
AbstractFileReference >> nextVersion [
	^ self resolve nextVersion
]

{ #category : #accessing }
AbstractFileReference >> numberOfHardLinks [
	^ self resolve numberOfHardLinks
]

{ #category : #streams }
AbstractFileReference >> openWritable: aBoolean [
	^ self resolve openWritable: aBoolean
]

{ #category : #delegated }
AbstractFileReference >> parent [
	^ self withPath: self resolve path parent
]

{ #category : #delegated }
AbstractFileReference >> parentUpTo: aParentDirName [
	^ self withPath: (self path parentUpTo: aParentDirName)
]

{ #category : #accessing }
AbstractFileReference >> pathSegments [
	^ self fullPath segments
]

{ #category : #delegated }
AbstractFileReference >> pathString [
	^ self resolve pathString
]

{ #category : #accessing }
AbstractFileReference >> permissions [
	"Return the FileSystemPermission for this node"
	^ self resolve permissions
]

{ #category : #streams }
AbstractFileReference >> readStream [
	self subclassResponsibility
]

{ #category : #streams }
AbstractFileReference >> readStreamDo: aBlock [
	| stream |
	stream := self readStream.
	^ [ aBlock value: stream ] 
		ensure: [ stream close ]
]

{ #category : #streams }
AbstractFileReference >> readStreamDo: doBlock ifAbsent: absentBlock [
	^ self isFile 
		ifTrue: [ self readStreamDo: doBlock ]
		ifFalse: absentBlock
]

{ #category : #streams }
AbstractFileReference >> readStreamEncoded: anEncoding [

	^ ZnCharacterReadStream
		on: self binaryReadStream
		encoding: anEncoding
]

{ #category : #streams }
AbstractFileReference >> readStreamEncoded: anEncoding do: aBlock [
	| stream |
	stream := self readStreamEncoded: anEncoding.
	^ [ aBlock value: stream ] 
		ensure: [ stream close ]
]

{ #category : #streams }
AbstractFileReference >> readStreamIfAbsent: absentBlock [
	^ self isFile
		ifTrue: [ self readStream ]
		ifFalse: absentBlock
]

{ #category : #navigating }
AbstractFileReference >> relativeTo: landmark [
	"Answer a new path relative to landmark."
	
	"parent/child/grandChild relativeTo: parent returns child/grandChild
	(Filesystem disk / 'griffle' / 'plonk' / 'nurp') relativeTo: (Filesystem disk / 'griffle') 
	returns plonk/nurp"

	^ landmark makeRelative: self resolve
]

{ #category : #navigating }
AbstractFileReference >> relativeToPath: landmarkPath [ 
	
	^ self fullPath relativeTo: landmarkPath
]

{ #category : #navigating }
AbstractFileReference >> relativeToReference: landmarkReference [
	"Return the path of the receiver relative to landmarkReference."
	
	^ self fullPath relativeTo: landmarkReference path
]

{ #category : #operations }
AbstractFileReference >> renameTo: newBasename [
	self subclassResponsibility
]

{ #category : #navigating }
AbstractFileReference >> resolve [
	^ self subclassResponsibility 
]

{ #category : #navigating }
AbstractFileReference >> resolve: anObject [
	^ anObject asResolvedBy: self
]

{ #category : #navigating }
AbstractFileReference >> resolvePath: aPath [
	^ self withPath: (self path resolvePath: aPath)
]

{ #category : #navigating }
AbstractFileReference >> resolveReference: aReference [
	^ aReference isAbsolute
		ifTrue: [ aReference ]
		ifFalse: [ self withPath: aReference path ]
]

{ #category : #navigating }
AbstractFileReference >> resolveString: aString [ 
	self subclassResponsibility
]

{ #category : #accessing }
AbstractFileReference >> size [
	"Return the size of the file in bytes."
	
	^ self resolve size
]

{ #category : #streams }
AbstractFileReference >> streamWritable: writable do: aBlock [
	^ writable 
		ifTrue: [ self writeStreamDo: aBlock ]
		ifFalse: [ self readStreamDo: aBlock ]
]

{ #category : #accessing }
AbstractFileReference >> targetPath [
	^ self resolve targetPath
]

{ #category : #accessing }
AbstractFileReference >> uid [
	^ self resolve uid
]

{ #category : #accessing }
AbstractFileReference >> uri [
	"Convert my path into a file:// type url. For odd characters use %20 notation."

	^ self asUrl
]

{ #category : #navigating }
AbstractFileReference >> withExtension: aString [
	^ self withPath: (self path withExtension: aString)
]

{ #category : #navigating }
AbstractFileReference >> withPath: newPath [
	^ self path == newPath
		ifTrue: [ self ]
		ifFalse: [ self copyWithPath: newPath ]
]

{ #category : #copying }
AbstractFileReference >> withoutExtension [
  "Returns the new reference based on receiver with fullname without its extension"

  ^  (self parent / self basenameWithoutExtension)
]

{ #category : #streams }
AbstractFileReference >> writeStream [
	self subclassResponsibility
]

{ #category : #streams }
AbstractFileReference >> writeStreamDo: aBlock [
	| stream |
	stream := self writeStream.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
]

{ #category : #streams }
AbstractFileReference >> writeStreamDo: doBlock ifPresent: presentBlock [
	^ self isFile
		ifTrue: presentBlock
		ifFalse: [ self writeStreamDo: doBlock ]
]

{ #category : #streams }
AbstractFileReference >> writeStreamEncoded: anEncoding [

	^ ZnCharacterWriteStream
		on: self binaryWriteStream
		encoding: anEncoding
]

{ #category : #streams }
AbstractFileReference >> writeStreamEncoded: anEncoding do: aBlock [
	| stream |
	stream := self writeStreamEncoded: anEncoding.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
]

{ #category : #streams }
AbstractFileReference >> writeStreamIfPresent: presentBlock [
	^ self isFile 
		ifTrue: presentBlock
		ifFalse: [ self writeStream ]
]
