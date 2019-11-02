"
DiskDirectoryEntry holds attributes for a single file / directory on the disk.

The actual attributes are held in two arrays which are returned directly from the #fileAttributes:mask: primitive.  See statAttributes and accessAttributes, below.

DiskDirectoryEntry's are not normally created directly, but are returned as a result of sending the #entry or #entries messages to a FileReference.

If the receiver's file is a symbolic link, the information contained in the receiver is for the target file, not the symbolic link.  See also DiskSymlinkDirectoryEntry.

 
Internal Representation and Key Implementation Points.

    Instance Variables
	accessAttributes:		<Array>  An array of booleans indicating whether the image has read, write and execute access to the file.
	statAttributes:		<Array>  See File class>>primFileAttributes:mask: for the list of attributes.


    Implementation Points

- statAttributes mostly corresponds with the information returned by the posix stat() call, thus its name.
- accessAttributes mostly corresponds with the information returned by the posix array() call, thus its name.

accessAttributes is relatively expensive to retrieve, so is loaded lazily only if requested.

"
Class {
	#name : #DiskDirectoryEntry,
	#superclass : #FileSystemDirectoryEntry,
	#instVars : [
		'statAttributes',
		'accessAttributes'
	],
	#classVars : [
		'Primitives'
	],
	#category : #'FileSystem-Core-Public'
}

{ #category : #testing }
DiskDirectoryEntry class >> exists: aPath in: aStore [
	"Answer a boolean indicating whether the supplied path exists"

	^aStore exists: aPath.
]

{ #category : #'instance creation' }
DiskDirectoryEntry class >> fileSystem: aFilesystem path: aPath [
	"Create a directory entry given a filesystem and a path in such filesystem."

	^ self reference: (aFilesystem referenceTo: aPath)
]

{ #category : #'instance creation' }
DiskDirectoryEntry class >> reference: aFileReference statAttributes: anArray [
	"Answer an instance of the receiver pre-loaded with the supplied attributes"

	^self new initializeWithReference: aFileReference statAttributes: anArray
]

{ #category : #'instance creation' }
DiskDirectoryEntry class >> store: aDiskStore path: aPath node: aNode [

	^(super store: aDiskStore path: aPath)
		statAttributes: (aNode at: 2);
		yourself.
]

{ #category : #'private - accessing' }
DiskDirectoryEntry >> accessAttributeMask [
	"Answer the mask used to retrieve access attributes"

	^2r10
]

{ #category : #'private - accessing' }
DiskDirectoryEntry >> accessAttributes [
	"Answer the access attributes of the receiver, i.e. whether the receiver is readable, writable and executable"

	^ accessAttributes ifNil: [ self getAccessAttributes ]
]

{ #category : #'private - accessing' }
DiskDirectoryEntry >> accessAttributes: anArray [

	accessAttributes := anArray
]

{ #category : #accessing }
DiskDirectoryEntry >> accessTime [

	^DateAndTime fromInternalTime: (self statAttributes at: 9)
]

{ #category : #accessing }
DiskDirectoryEntry >> accessUnixTime [
	"Answer the receivers access time in Unix time_t format"

	^self accessTime asUnixTime
]

{ #category : #'private - accessing' }
DiskDirectoryEntry >> allAttributeMask [
	"Answer the mask used to retrieve all attributes"

	^self accessAttributeMask bitOr: self statAttributeMask
]

{ #category : #accessing }
DiskDirectoryEntry >> asFilePluginNode [
	"Backward compatibility: Answer an array of file attributes matching that returned by FilePlugin"

	^{
		self basename.
		(self statAttributes at: 12) ifNil: [ self statAttributes at: 11 ].
		self statAttributes at: 10.
		self isDirectory.
		self size.
		self posixPermissions.
		self isSymlink.
	}
]

{ #category : #accessing }
DiskDirectoryEntry >> changeTime [
	"Answer the time the receivers metadata was last changed.
	On platforms that don't support change time, use the modification time"

	^DateAndTime fromInternalTime: ((self statAttributes at: 11) ifNil: [ self statAttributes at: 12 ])

]

{ #category : #accessing }
DiskDirectoryEntry >> changeUnixTime [
	"Answer the receiver's change time in Unix time_t format"

	^self changeTime asUnixTime

]

{ #category : #accessing }
DiskDirectoryEntry >> creationTime [
	"Answer the receivers creation time.  If this is not available, answer the change time, i.e. the time at which the receivers metadata was last updated."

	^DateAndTime fromInternalTime: ((self statAttributes at: 12) ifNil: [ self statAttributes at: 11 ])
]

{ #category : #accessing }
DiskDirectoryEntry >> creationUnixTime [
	"Answer the receiver's creation time in Unix time_t format"

	^self creationTime asUnixTime
]

{ #category : #accessing }
DiskDirectoryEntry >> deviceId [
	"Answer the device ID of the receiver"
	
	^self statAttributes at: 4
]

{ #category : #'cache management' }
DiskDirectoryEntry >> getAccessAttributes [
	"Store the receivers access() attributes for future reference"

	^accessAttributes := self primitives fileAttributes: self pathString mask: self accessAttributeMask
]

{ #category : #'cache management' }
DiskDirectoryEntry >> getAllAttributes [
	"Store all the receivers attributes for future reference"

	| allAttributes |
	
	allAttributes := self primitives fileAttributes: self pathString mask: self allAttributeMask.
	statAttributes := allAttributes at: 1.
	accessAttributes := allAttributes at: 2.

]

{ #category : #'cache management' }
DiskDirectoryEntry >> getStatAttributes [
	"Store the receivers stat() attributes for future reference"

	^statAttributes := self primitives fileAttributes: self pathString mask: self statAttributeMask.

]

{ #category : #accessing }
DiskDirectoryEntry >> gid [
	"Answer the GID of the owner of the receiver"
	
	^self statAttributes at: 7
]

{ #category : #inspecting }
DiskDirectoryEntry >> gtInspectAttributesIn: composite [
	<gtInspectorPresentationOrder: 2>

	| attrDictionary |

	attrDictionary := Dictionary new.
	self class methodsDo: [ :method |
		method methodNode arguments size = 0 ifTrue:
			[ attrDictionary at: method selector put:
				([ (method selector value: self) ]
					on: Error
					do: [ 'N/A' translated ]) ] ].
	^(attrDictionary gtInspectorItemsIn: composite)
		when: [ attrDictionary isNotEmpty ];
		title: 'Attributes' translated;
		yourself.
]

{ #category : #testing }
DiskDirectoryEntry >> hasCreationTime [
	"Answer a boolean indicating whether the receiver has a creation timestamp, or is just using the change timestamp."

	^(self statAttributes at: 12) isNotNil
]

{ #category : #initialization }
DiskDirectoryEntry >> initializeWithReference: aFileReference statAttributes: anArray [

	super initialize.	
	reference := aFileReference.
	statAttributes := anArray
]

{ #category : #accessing }
DiskDirectoryEntry >> inode [
	"Answer the index number of the receiver"
	
	^self statAttributes at: 3
]

{ #category : #testing }
DiskDirectoryEntry >> isBlock [
	"Answer true if the receiver is a block device"
	
	^self primitives modeIsBlock: self mode.
]

{ #category : #testing }
DiskDirectoryEntry >> isCharacter [
	"Answer true if the receiver is a character device"
	
	^self primitives modeIsCharacter: self mode
]

{ #category : #testing }
DiskDirectoryEntry >> isDirectory [
	"Answer true if the receiver is a directory"

	reference isRoot ifTrue: [ ^true ].
	^[ self primitives modeIsDirectory: self mode ]
		on: FileDoesNotExistException
		do: [ false ].
]

{ #category : #testing }
DiskDirectoryEntry >> isExecutable [
	"Answer a boolean indicating whether the VM can execute the receivers file"

	^self accessAttributes at: 3
]

{ #category : #testing }
DiskDirectoryEntry >> isFIFO [
	"Answer true if the receiver is FIFO"

	^self primitives modeIsFIFO: self mode
]

{ #category : #testing }
DiskDirectoryEntry >> isFile [
	"Answer true if the receiver is a file (exists and is not a directory)"
	
	reference isRoot ifTrue: [ ^false ].
	^[ (self primitives modeIsDirectory: self mode) not ]
		on: FileDoesNotExistException
		do: [ false ].
]

{ #category : #testing }
DiskDirectoryEntry >> isHidden [
	"Answer a boolean indicating whether the receiver is hidden"

	^reference fileSystem isHidden: reference attributes: self statAttributes
]

{ #category : #testing }
DiskDirectoryEntry >> isReadable [
	"Answer a boolean indicating whether the VM can read the receivers file"

	^self accessAttributes at: 1
]

{ #category : #testing }
DiskDirectoryEntry >> isRegular [
	"Answer true if the receiver is a regular file"
	
	^self primitives modeIsRegular: self mode
]

{ #category : #testing }
DiskDirectoryEntry >> isSocket [
	"Answer true if the receiver is a socket"
	
	^self primitives modeIsSocket: self mode
]

{ #category : #testing }
DiskDirectoryEntry >> isSymlink [
	"Answer true if the receiver is a symbolic link"

	reference path isRoot ifTrue: [ ^false ].
	"Don't use cached information as the values might reflect a target file accessed from a symlink."
	^self primitives isSymlink: self pathString
]

{ #category : #testing }
DiskDirectoryEntry >> isWritable [
	"Answer a boolean indicating whether the VM can write to the receivers file"

	^self accessAttributes at: 2
]

{ #category : #accessing }
DiskDirectoryEntry >> mode [
	"Answer the mode of the receiver"
	
	^self statAttributes at: 2
]

{ #category : #accessing }
DiskDirectoryEntry >> modificationTime [
	"Answer the receiver's modification time"

	^DateAndTime fromInternalTime: (self statAttributes at: 10)
]

{ #category : #accessing }
DiskDirectoryEntry >> modificationUnixTime [
	"Answer the receiver's modification time in Unix time_t format"

	^self modificationTime asUnixTime
]

{ #category : #accessing }
DiskDirectoryEntry >> numberOfHardLinks [
	"Answer the number of hard links to the receiver"
	
	^(self statAttributes at: 5) ifNil:
		[ FileAttributeNotSupported signalWith: reference ]
]

{ #category : #accessing }
DiskDirectoryEntry >> permissions [
	"Answer the receivers permissions"
	
	^FileSystemPermission posixPermissions: self posixPermissions
]

{ #category : #accessing }
DiskDirectoryEntry >> posixPermissions [
	"Answer the receivers posix permissions"
	
	^self mode bitAnd: 8r777
]

{ #category : #'private - accessing' }
DiskDirectoryEntry >> primitives [
	"Answer the attribute primitives to be used by the receiver."

	^File
]

{ #category : #accessing }
DiskDirectoryEntry >> size [
	"Answer the number of bytes in the receiver"
	
	^self statAttributes at: 8
]

{ #category : #'private - accessing' }
DiskDirectoryEntry >> statAttributeMask [
	"Answer the mask used to retrieve stat() attributes"

	^2r1
]

{ #category : #'private - accessing' }
DiskDirectoryEntry >> statAttributes [
	^ statAttributes ifNil: [ self getStatAttributes ]
]

{ #category : #'private - accessing' }
DiskDirectoryEntry >> statAttributes: anArray [

	statAttributes := anArray
]

{ #category : #accessing }
DiskDirectoryEntry >> uid [
	"Answer the UID of the owner of the receiver"
	
	^self statAttributes at: 6
]
