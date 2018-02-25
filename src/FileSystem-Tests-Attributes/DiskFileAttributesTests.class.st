"
Try and test the FileAttributes and FileAttributesPluginPrims classes.

As Pharo doesn't provide a mechanism to set the attributes, pick a few well known files and make sure they have reasonable attributes, e.g. the VM is executable, it isn't older than when the classes were first created or newer than today, etc.

"
Class {
	#name : #DiskFileAttributesTests,
	#superclass : #TestCase,
	#instVars : [
		'primitives',
		'temporaryFile'
	],
	#category : #'FileSystem-Tests-Attributes'
}

{ #category : #resources }
DiskFileAttributesTests class >> resources [

	^Array with: DiskFileAttributesTestsResources.
]

{ #category : #'helper methods' }
DiskFileAttributesTests >> attributesCollection [
	"Answer an array of FileAttributes for the test resource file.
	This is currently a single entry, but leaving the infrastructure in place in case it is extended."
	
	^{
		self tempFileEntry.
	}
]

{ #category : #'helper methods' }
DiskFileAttributesTests >> attributesCollectionDo: aBlock [
	"Evaluate the supplied block for each of the attribute cache modes"

	^self attributesCollection do: aBlock
]

{ #category : #'helper methods' }
DiskFileAttributesTests >> collectionAssert: aBlock [
	"Evaluate the assertion block for each of the FileAttribute cache modes"

	self attributesCollectionDo:
		[ :each | self assert: (aBlock value: each) ].
]

{ #category : #'helper methods' }
DiskFileAttributesTests >> collectionAssert: aBlock equals: expected [
	"Evaluate the assertion block for each of the FileAttribute cache modes"

	self attributesCollectionDo:
		[ :each | self assert: (aBlock value: each) equals: expected ].
]

{ #category : #running }
DiskFileAttributesTests >> setUp [

	primitives := FileAttributesPluginPrims new.
	
]

{ #category : #'helper methods' }
DiskFileAttributesTests >> tempFileEntry [

	| file |
	
	file := self tempFileResource file resolve.
	^DiskDirectoryEntry reference: file.
]

{ #category : #'helper methods' }
DiskFileAttributesTests >> tempFileResource [

	^DiskFileAttributesTestsResources current
]

{ #category : #'tests - single attribute' }
DiskFileAttributesTests >> testAccessTime [

	| resource accessTime |
	resource := self tempFileResource.
	self attributesCollectionDo: [ :each |
		accessTime := each accessTime.
		self assert: resource beforeCreationTime <= accessTime.
		self assert: resource afterCreationTime >= accessTime.
		].
]

{ #category : #tests }
DiskFileAttributesTests >> testCreationTime [

	| resource creationTime |
	resource := self tempFileResource.
	self attributesCollectionDo: [ :each |
		creationTime := each creationTime.
		self assert: resource beforeCreationTime <= creationTime.
		self assert: resource afterCreationTime >= creationTime.
		].
]

{ #category : #tests }
DiskFileAttributesTests >> testExists [

	| file |
	file := self tempFileResource file resolve.
	self assert: (DiskDirectoryEntry exists: file path in: file fileSystem)
]

{ #category : #tests }
DiskFileAttributesTests >> testExistsNot [

	| file |
	file := '/thisfileshouldnot.exist' asFileReference.
	self assert: (DiskDirectoryEntry exists: file path in: file fileSystem) not.

]

{ #category : #tests }
DiskFileAttributesTests >> testFileSize [

	self collectionAssert: [ :each | each size ] equals: 72.
]

{ #category : #tests }
DiskFileAttributesTests >> testIsBlock [

	self collectionAssert: [ :each | each isBlock ] equals: false.
]

{ #category : #tests }
DiskFileAttributesTests >> testIsCharacter [

	self collectionAssert: [ :each | each isCharacter ] equals: false.
]

{ #category : #tests }
DiskFileAttributesTests >> testIsDirectory [

	| file |

	self collectionAssert: [ :each | each isDirectory ] equals: false.
	file := FileLocator temp resolve.
	self assert: (DiskDirectoryEntry reference: file) isDirectory.
]

{ #category : #tests }
DiskFileAttributesTests >> testIsExecutable [

	| file filesystem path |
	file := FileLocator vmBinary resolve.
	filesystem := file fileSystem.
	path := file path.
	self assert: (DiskDirectoryEntry reference: file) isExecutable.

]

{ #category : #tests }
DiskFileAttributesTests >> testIsFIFO [

	self collectionAssert: [ :each | each isFIFO ] equals: false.
]

{ #category : #tests }
DiskFileAttributesTests >> testIsReadable [

	self collectionAssert: [ :each | each isReadable ].
]

{ #category : #tests }
DiskFileAttributesTests >> testIsRegular [

	self collectionAssert: [ :each | each isRegular ]
]

{ #category : #tests }
DiskFileAttributesTests >> testIsSocket [

	self collectionAssert: [ :each | each isSocket ] equals: false.
]

{ #category : #tests }
DiskFileAttributesTests >> testIsSymlink [

	self collectionAssert: [ :each | each isSymlink ] equals: false.
]

{ #category : #tests }
DiskFileAttributesTests >> testIsWriteable [

	self collectionAssert: [ :each | each isWritable ].
]

{ #category : #tests }
DiskFileAttributesTests >> testModificationTime [

	| resource modificationTime |
	resource := self tempFileResource.
	self attributesCollectionDo: [ :each |
		modificationTime := each modificationTime.
		self assert: resource beforeCreationTime <= modificationTime.
		self assert: resource afterCreationTime >= modificationTime.
		].
]

{ #category : #tests }
DiskFileAttributesTests >> testNLink [
	"Assume that there is only one hard link to the just created temporary file"

	self collectionAssert: [ :each | each numberOfHardLinks ] equals: 1.
]

{ #category : #tests }
DiskFileAttributesTests >> testTargetFile [

	"The temporary file isn't a symbolic link, so the targetFile is not supported"
	self attributesCollectionDo: [ :each |
		self should: [ each targetFileReference ] raise: FileAttributeNotSupported ].

]
