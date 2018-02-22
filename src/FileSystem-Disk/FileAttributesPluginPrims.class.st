"
FileAttributesPluginPrims provides primitives to:

- Check the existence of files (#primExists:)
- Retrieve information returned by the libc stat() and access() functions (#primFileAttributes:mask:, #primFileAttribute:number:)
- Retrieve masks used by stat.st_mode (#primFileMasks)

Public API and Key Messages

- primExists: - check whether the given file exists
- primFileAttributes:mask: - retrieve stat() and/or access() information
- primFileAttribute:number: - retrieve a single stat() or access() value.
- primFileMasks - retrieve stat.st_mode masks

See the method comments for more information.

E.g. to check file existance:

FileAttributesPluginPrims new primExists: '/bin/bash'
 
    Implementation Points

File attribute information was originally retrieved through the FilePlugin, which had a single primitive to test for file existence and return some file attributes.  As the amount of information returned increased over time, the performance of the #exists method progressively deteriated as all the additional information was retrieved, but just discarded.

The current design of FileAttributesPlugin is based on the following observations and assumptions:

- The most often called method is checking file existence (by a factor of 3 or 4).
- Most other callers retireve and use a single piece of information.
- Named primitive calls are relatively expensive, so it makes sense to return as much information as possible.
- Calling access() is relatively expensive.
- We can't make assumptions about how long it is reasonable to cache any attribute information, since it can be changed externally to Pharo.
- The attribute information is normally very short lived.

Based on this, FileAttributesPlugin implements the three access modes as described earlier in the comments.

"
Class {
	#name : #FileAttributesPluginPrims,
	#superclass : #Object,
	#classVars : [
		'Encoder',
		'S_IFBLK',
		'S_IFCHR',
		'S_IFDIR',
		'S_IFIFO',
		'S_IFLNK',
		'S_IFMT',
		'S_IFREG',
		'S_IFSOCK'
	],
	#category : #'FileSystem-Disk'
}

{ #category : #initialize }
FileAttributesPluginPrims class >> initialize [

	Encoder := ZnCharacterEncoder newForEncoding: 'utf8'.
	self reset.
]

{ #category : #initialize }
FileAttributesPluginPrims class >> reset [
	"Reload the masks"

	| masks |

	masks := self new primFileMasks.
	S_IFMT := masks at: 1.
	S_IFSOCK := masks at: 2.
	S_IFLNK := masks at: 3.
	S_IFREG := masks at: 4.
	S_IFBLK := masks at: 5.
	S_IFDIR := masks at: 6.
	S_IFCHR := masks at: 7.
	S_IFIFO := masks at: 8.

]

{ #category : #errors }
FileAttributesPluginPrims >> cantAllocateMemory [

	^-10
]

{ #category : #errors }
FileAttributesPluginPrims >> cantOpenDir [

	^-9
]

{ #category : #errors }
FileAttributesPluginPrims >> cantReadlink [

	^-8
]

{ #category : #errors }
FileAttributesPluginPrims >> cantStatPath [
	"SecurityPlugin determined that the requested path cannot be accessed."
	^-3
]

{ #category : #errors }
FileAttributesPluginPrims >> corruptValue [

	^-7
]

{ #category : #'private - encoding' }
FileAttributesPluginPrims >> decode: aString [

	^ (ZnCharacterEncoder newForEncoding: 'utf8') decodeBytes: aString asByteArray
]

{ #category : #'private - encoding' }
FileAttributesPluginPrims >> encode: aString [

	^ ((ZnCharacterEncoder newForEncoding: 'utf8') encodeString: aString) asString
]

{ #category : #testing }
FileAttributesPluginPrims >> exists: aString [

	^self primExists: (self encode: aString)
]

{ #category : #attributes }
FileAttributesPluginPrims >> fileAttribute: aString number: attributeNumber [
	"Answer the attribute identified by attributeNumber for the specified file (aString).
	For backward compatibility (on Unix) with FileReference if the file doesn't exist, and the specified path is a (broken) symbolic link, answer the requested attribute for the symbolic link."

	^self primFileAttribute: (self encode: aString) number: attributeNumber
]

{ #category : #private }
FileAttributesPluginPrims >> fileAttributeNumberMap [
	"Answer a mapping of attribute number to name for #primFileAttribute:number:"
	
	^#(
		#targetName
		#mode
		#inode
		#deviceId
		#numberOfHardLinks
		#uid
		#gid
		#size
		#accessUnixTime
		#modificationUnixTime
		#changeUnixTime
		#creationUnixTime
		#isReadable
		#isWritable
		#isExecutable
		#isSymlink
		)
]

{ #category : #attributes }
FileAttributesPluginPrims >> fileAttributes: aString mask: attributeMask [

	^self primFileAttributes: (self encode: aString) mask: attributeMask
]

{ #category : #'private - encoding' }
FileAttributesPluginPrims >> fixPath: pathString [
	"An empty path string is assumed to refer to the current directory. opendir()
	on an empty string will fail, so give it an explicit path to the current
	directory."

	^ pathString isEmpty
		ifTrue: ['.']
		ifFalse: [pathString]
]

{ #category : #errors }
FileAttributesPluginPrims >> getAttributesFailed [

	^-4
]

{ #category : #errors }
FileAttributesPluginPrims >> invalidArguments [

	^-6
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isBlock: aPath [
	"Answer a boolean indicating whether the supplied path is a Block file"

	^self modeIsBlock: (self modeOf: aPath)

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isCharacter: aPath [
	"Answer a boolean indicating whether the supplied path is a Character file"

	^self modeIsCharacter: (self modeOf: aPath)

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isDirectory: aPath [
	"Answer a boolean indicating whether the supplied path is a Directory"

	^[ self modeIsDirectory: (self modeOf: aPath) ]
		on: FileDoesNotExist
		do: [ false ].
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isExecutable: aPath [
	"Answer a boolean indicating whether the supplied path is a executable file"

	^self fileAttribute: aPath number: 15

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isFIFO: aPath [
	"Answer a boolean indicating whether the supplied path is a FIFO file"

	^self modeIsFIFO: (self modeOf: aPath)

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isFile: aPathString [
	"Answer a boolean indicating whether the supplied path is a file, i.e. not a directory"

	^[ (self modeIsDirectory: (self modeOf: aPathString)) not ]
		on: FileDoesNotExist
		do: [ false ].
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isReadable: aPath [
	"Answer a boolean indicating whether the supplied path is a readable file"

	^self fileAttribute: aPath number: 13

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isRegular: aPath [
	"Answer a boolean indicating whether the supplied path is a Regular file"

	^self modeIsRegular: (self modeOf: aPath)

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isSocket: aPath [
	"Answer a boolean indicating whether the supplied path is a Socket file"

	^self modeIsSocket: (self modeOf: aPath)

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isSymlink: aPath [
	"Answer a boolean indicating whether the supplied path is a Symlink file"

	^self fileAttribute: aPath number: 16

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> isWritable: aPath [
	"Answer a boolean indicating whether the supplied path is a writable file"

	^self fileAttribute: aPath number: 14

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> modeIsBlock: aMode [
	"Answer a boolean indicating whether the supplied mode has the Block type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFBLK
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> modeIsCharacter: aMode [
	"Answer a boolean indicating whether the supplied mode has the Character type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFCHR
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> modeIsDirectory: aMode [
	"Answer a boolean indicating whether the supplied mode has the Directory type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFDIR
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> modeIsFIFO: aMode [
	"Answer a boolean indicating whether the supplied mode has the FIFO type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFIFO
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> modeIsRegular: aMode [
	"Answer a boolean indicating whether the supplied mode has the Regular type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFREG
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> modeIsSocket: aMode [
	"Answer a boolean indicating whether the supplied mode has the Socket type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFSOCK
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> modeIsSymlink: aMode [
	"Answer a boolean indicating whether the supplied mode has the Socket type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFLNK
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> modeOf: aPath [
	"Answer the statBuf.st_mode for the supplied path"
	
	^self fileAttribute: aPath number: 2
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> permissions: aPath [
	"Answer the FileSystemPermission's for the supplied path"

	^FileSystemPermission posixPermissions: (self posixPermissions: aPath)

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> posixPermissions: aPath [
	"Answer the posix permissions for the supplied path"

	^(self modeOf: aPath) bitAnd: 8r777

]

{ #category : #primitives }
FileAttributesPluginPrims >> primClosedir: directoryPointerBytes [
	"Close the directory stream associated with directoryPointerBytes.
	Caution: do not call this twice on the same externalAddress."

	"self primClosedir: (self primOpendir: '/etc')"
	"self primClosedir: (self primOpendir: '/no/such/directory')"

	<primitive: 'primitiveClosedir' module: 'FileAttributesPlugin' error: error>
	^self primitiveFailed.
]

{ #category : #primitives }
FileAttributesPluginPrims >> primExists: aString [
	"Answer a boolean indicating whether the supplied file exists."

	<primitive: 'primitiveFileExists' module: 'FileAttributesPlugin' error: error>
	^(error isPrimitiveError)
		ifTrue: [ self signalError: error errorCode for: aString ]
		ifFalse: [ self primitiveFailed ].
]

{ #category : #primitives }
FileAttributesPluginPrims >> primFileAttribute: aString number: attributeNumber [
	"Answer a single attribute for the supplied file.
	For backward compatibility (on Unix) with FileReference if the file doesn't exist, and the specified path is a (broken) symbolic link, answer the requested attribute for the symbolic link.

stat() information:
	
	1: name
	2: mode
	3: ino
	4: dev
	5: nlink
	6: uid
	7: gid
	8: size
	9: accessDate
	10: modifiedDate
	11: changeDate
	12: creationDate

access() information

	13: is readable
	14: is writeable
	15: is executable

symbolic link information

	16: is symbolic link
	"
	<primitive: 'primitiveFileAttribute' module: 'FileAttributesPlugin' error: error>
	^error isPrimitiveError ifTrue: [ 
		(error errorCode = self cantStatPath and: [ attributeNumber ~= 16 ]) ifTrue: 
			[ (self fileAttributeNumberMap at: attributeNumber) value: 
				(DiskSymlinkDirectoryEntry fileSystem: DiskStore currentFileSystem path: aString asPath) ]
		ifFalse:
			[ self signalError: error errorCode for: aString ] ]
	ifFalse:
		[ self primitiveFailed ]
]

{ #category : #primitives }
FileAttributesPluginPrims >> primFileAttributes: aString mask: attributeMask [
	"Answer an array of attributes for the supplied file.  The size and contents of the array are determined by the attributeMask:

Bit 0: stat() information
Bit 1: access() information
Bit 2: use lstat() (instead of stat())

On error, answer an error code (Integer).

stat() information:
	
	1: name
	2: mode
	3: ino
	4: dev
	5: nlink
	6: uid
	7: gid
	8: size
	9: accessDate
	10: modifiedDate
	11: creationDate

access() information

	1: is readable
	2: is writeable
	3: is executable
	"
	<primitive: 'primitiveFileAttributes' module: 'FileAttributesPlugin' error: error>
	^error isPrimitiveError ifTrue:
		[ self signalError: error errorCode for: aString ]
	ifFalse: 
		[ self primitiveFailed ]
]

{ #category : #primitives }
FileAttributesPluginPrims >> primFileMasks [
	"Answer an array of well known masks:
	
	1: S_IFMT
	2: S_IFSOCK
	3: S_IFLNK
	4: S_IFREG
	5: S_IFBLK
	6: S_IFDIR
	7: S_IFCHR
	8: S_IFIFO

	For more information, see: http://man7.org/linux/man-pages/man2/stat.2.html
	"
	<primitive: 'primitiveFileMasks' module: 'FileAttributesPlugin' error: error>
	^self primitiveFailed
]

{ #category : #primitives }
FileAttributesPluginPrims >> primLogicalDrives [
	"Answer the windows logical drive mask"
	<primitive: 'primitiveLogicalDrives' module: 'FileAttributesPlugin' error: error>
	^self primitiveFailed
]

{ #category : #primitives }
FileAttributesPluginPrims >> primOpendir: pathString [
	"Answer an ExternalAddress for a directory stream on pathString, or nil if
	the directory cannot be opened"

	"self primOpendir: '/etc'"
	"self primOpendir: '.'"
	"self primOpendir: '/no/such/directory'"

	<primitive: 'primitiveOpendir' module: 'FileAttributesPlugin' error: error>
	^self primitiveFailed
]

{ #category : #primitives }
FileAttributesPluginPrims >> primPathMax [
	"Answer the VMs PATH_MAX value"
	<primitive: 'primitivePathMax' module: 'FileAttributesPlugin' error: error>
	^self primitiveFailed
]

{ #category : #primitives }
FileAttributesPluginPrims >> primReaddir: directoryPointerBytes [
	"Read the next directory entry from the directory stream associated with
	directoryPointerBytes. Answer the name of the entry, ornil for end of directory stream."

	"self primReaddir: (self primOpendir: '/etc')"
	"self primReaddir: (self primOpendir: '/no/such/directory')"

	<primitive: 'primitiveReaddir' module: 'FileAttributesPlugin' error: error>
	^self primitiveFailed
]

{ #category : #primitives }
FileAttributesPluginPrims >> primRewinddir: directoryPointerBytes [
	"Rewind the directory stream associated with directoryPointerBytes. Answer
	anExternalAddress on success, or nil on failure."

	"self primRewinddir: (self primOpendir: '/etc')"
	"self primRewinddir: (self primOpendir: '/no/such/directory')"

	<primitive: 'primitiveRewinddir' module: 'FileAttributesPlugin' error: error>
	^self primitiveFailed

]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> s_IFBLK [
	^ S_IFBLK
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> s_IFCHR [
	^ S_IFCHR
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> s_IFDIR [
	^ S_IFDIR
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> s_IFIFO [
	^ S_IFIFO
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> s_IFLNK [
	^ S_IFLNK
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> s_IFMT [
	^ S_IFMT
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> s_IFREG [
	^ S_IFREG
]

{ #category : #'mode bits' }
FileAttributesPluginPrims >> s_IFSOCK [
	^ S_IFSOCK
]

{ #category : #private }
FileAttributesPluginPrims >> signalError: aNumber for: aString [
	"Raise the appropriate signal for the supplied error number"

	aNumber = self stringTooLong ifTrue: 
		[ ^IllegalName signalWith: aString ].
	aNumber = self cantStatPath ifTrue: 
		[ ^FileDoesNotExist signalWith: aString ].
	aNumber = self getAttributesFailed ifTrue: 
		[ ^FileDoesNotExist signalWith: aString ].
	^FileSystemError signalWith: aString.
]

{ #category : #errors }
FileAttributesPluginPrims >> statFailed [
	"A call to stat() failed"
	^-2
]

{ #category : #errors }
FileAttributesPluginPrims >> stringTooLong [
	"String too long.  A file path name was longer than PATH_MAX"
	^-1
]

{ #category : #errors }
FileAttributesPluginPrims >> timeConversionFailed [

	^-5
]

{ #category : #versions }
FileAttributesPluginPrims >> versionString [

	<primitive: 'primitiveVersionString' module: 'FileAttributesPlugin' error: error>
	^self primitiveFailed
]
