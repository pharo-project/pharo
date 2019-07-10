"
Try and test file attribute access from FileReference.

As Pharo doesn't provide a mechanism to set the attributes, pick a few well known files and make sure they have reasonable attributes, e.g. the VM is executable, it isn't older than when the classes were first created or newer than today, etc.

"
Class {
	#name : #FileReferenceAttributeTest,
	#superclass : #TestCase,
	#instVars : [
		'primitives'
	],
	#category : #'FileSystem-Tests-Attributes-Tests'
}

{ #category : #resources }
FileReferenceAttributeTest class >> resources [

	^Array with: DiskFileAttributesTestsResource.
]

{ #category : #running }
FileReferenceAttributeTest >> setUp [

	super setUp.
	primitives := File.
	
]

{ #category : #private }
FileReferenceAttributeTest >> tempFileResource [

	^DiskFileAttributesTestsResource current
]

{ #category : #tests }
FileReferenceAttributeTest >> testAccessTime [

	| resource accessTime |
	resource := self tempFileResource.
	accessTime := resource file accessTime.
	self assert: resource beforeCreationTime <= accessTime.
	self assert: resource afterCreationTime >= accessTime.
	self assert: resource file entry accessTime equals: accessTime.

]

{ #category : #tests }
FileReferenceAttributeTest >> testChangeTime [

	| resource changeTime |
	resource := self tempFileResource.
	changeTime := resource file changeTime.
	self assert: resource beforeCreationTime <= changeTime.
	self assert: resource afterCreationTime >= changeTime.
	self assert: resource file entry changeTime equals: changeTime.

]

{ #category : #tests }
FileReferenceAttributeTest >> testCreationTime [

	| resource creationTime |
	resource := self tempFileResource.
	creationTime := resource file creationTime.
	self assert: resource beforeCreationTime <= creationTime.
	self assert: resource afterCreationTime >= creationTime.
	self assert: resource file entry creationTime equals: creationTime.

]

{ #category : #tests }
FileReferenceAttributeTest >> testExists [

	self assert: self tempFileResource file exists
]

{ #category : #tests }
FileReferenceAttributeTest >> testFileSize [

	self assert: self tempFileResource file size equals: 72.
	self assert: self tempFileResource file entry size equals: self tempFileResource file size.

]

{ #category : #tests }
FileReferenceAttributeTest >> testIsBlock [

	self deny: self tempFileResource file isBlock.
	self assert: self tempFileResource file entry isBlock equals: self tempFileResource file isBlock.

]

{ #category : #tests }
FileReferenceAttributeTest >> testIsCharacter [

	self deny: self tempFileResource file isCharacter.
	self assert: self tempFileResource file entry isCharacter equals: self tempFileResource file isCharacter.

]

{ #category : #tests }
FileReferenceAttributeTest >> testIsDirectory [

	self assert: self tempFileResource file isDirectory equals: false.
	self assert: self tempFileResource file entry isDirectory equals: self tempFileResource file isDirectory.
	self assert: FileLocator temp isDirectory.
]

{ #category : #tests }
FileReferenceAttributeTest >> testIsExecutable [

	self assert: FileLocator vmBinary isExecutable.
	self assert: FileLocator vmBinary entry isExecutable equals: FileLocator vmBinary isExecutable
]

{ #category : #tests }
FileReferenceAttributeTest >> testIsFIFO [

	self assert: self tempFileResource file isFIFO equals: false.
	self assert: self tempFileResource file entry isFIFO equals: self tempFileResource file isFIFO.
]

{ #category : #tests }
FileReferenceAttributeTest >> testIsFile [

	self assert: self tempFileResource file isFile.
	self assert: self tempFileResource file entry isFile equals: self tempFileResource file isFile.
	self deny: FileLocator temp isFile.
]

{ #category : #tests }
FileReferenceAttributeTest >> testIsReadable [

	self assert: self tempFileResource file isReadable.
	self assert: self tempFileResource file entry isReadable equals: self tempFileResource file isReadable.
]

{ #category : #tests }
FileReferenceAttributeTest >> testIsRegular [

	self assert: self tempFileResource file isRegular.
	self assert: self tempFileResource file entry isRegular equals: self tempFileResource file isRegular
]

{ #category : #tests }
FileReferenceAttributeTest >> testIsSocket [

	self assert: self tempFileResource file isSocket equals: false.
	self assert: self tempFileResource file entry isSocket equals: self tempFileResource file isSocket.
]

{ #category : #tests }
FileReferenceAttributeTest >> testIsSymlink [

	self assert: self tempFileResource file isSymlink equals: false.
	self assert: self tempFileResource file entry isSymlink equals: self tempFileResource file isSymlink.
]

{ #category : #tests }
FileReferenceAttributeTest >> testIsWriteable [

	self assert: self tempFileResource file isWritable.
	self assert: self tempFileResource file entry isWritable equals: self tempFileResource file isWritable.
]

{ #category : #tests }
FileReferenceAttributeTest >> testModificationTime [

	| resource modificationTime |
	resource := self tempFileResource.
	modificationTime := resource file modificationTime.
	self assert: resource beforeCreationTime <= modificationTime.
	self assert: resource afterCreationTime >= modificationTime.
	self assert: resource file entry modificationTime equals: modificationTime.

]

{ #category : #tests }
FileReferenceAttributeTest >> testNLink [
	"Assume that there is only one hard link to the just created temporary file"

	OSPlatform current name = #'Win32' ifTrue: 
		[ self should: [ self tempFileResource file numberOfHardLinks ] raise: FileAttributeNotSupported ]
	ifFalse:
		[ self assert: self tempFileResource file numberOfHardLinks equals: 1.
		self assert: self tempFileResource file entry numberOfHardLinks equals: 1 ].
]

{ #category : #tests }
FileReferenceAttributeTest >> testTargetFile [

	"The temporary file isn't a symbolic link, so the targetFile isn't supported"
	self should: [ self tempFileResource file targetPath ] raise: FileAttributeNotSupported.
	self should: [ self tempFileResource file entry targetPath ] raise: FileAttributeNotSupported.
]
