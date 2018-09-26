"
Try and test file attribute access from FileReference.

As Pharo doesn't provide a mechanism to set the attributes, pick a few well known files and make sure they have reasonable attributes, e.g. the VM is executable, it isn't older than when the classes were first created or newer than today, etc.

"
Class {
	#name : #FileReferenceAttributeTests,
	#superclass : #TestCase,
	#instVars : [
		'primitives'
	],
	#category : #'FileSystem-Tests-Attributes'
}

{ #category : #resources }
FileReferenceAttributeTests class >> resources [

	^Array with: DiskFileAttributesTestsResources.
]

{ #category : #running }
FileReferenceAttributeTests >> setUp [

	super setUp.
	primitives := File.
	
]

{ #category : #private }
FileReferenceAttributeTests >> tempFileResource [

	^DiskFileAttributesTestsResources current
]

{ #category : #tests }
FileReferenceAttributeTests >> testAccessTime [

	| resource accessTime |
	resource := self tempFileResource.
	accessTime := resource file accessTime.
	self assert: resource beforeCreationTime <= accessTime.
	self assert: resource afterCreationTime >= accessTime.
	self assert: resource file entry accessTime equals: accessTime.

]

{ #category : #tests }
FileReferenceAttributeTests >> testChangeTime [

	| resource changeTime |
	resource := self tempFileResource.
	changeTime := resource file changeTime.
	self assert: resource beforeCreationTime <= changeTime.
	self assert: resource afterCreationTime >= changeTime.
	self assert: resource file entry changeTime equals: changeTime.

]

{ #category : #tests }
FileReferenceAttributeTests >> testCreationTime [

	| resource creationTime |
	resource := self tempFileResource.
	creationTime := resource file creationTime.
	self assert: resource beforeCreationTime <= creationTime.
	self assert: resource afterCreationTime >= creationTime.
	self assert: resource file entry creationTime equals: creationTime.

]

{ #category : #tests }
FileReferenceAttributeTests >> testExists [

	self assert: self tempFileResource file exists
]

{ #category : #tests }
FileReferenceAttributeTests >> testFileSize [

	self assert: self tempFileResource file size equals: 72.
	self assert: self tempFileResource file entry size equals: self tempFileResource file size.

]

{ #category : #tests }
FileReferenceAttributeTests >> testIsBlock [

	self deny: self tempFileResource file isBlock.
	self assert: self tempFileResource file entry isBlock equals: self tempFileResource file isBlock.

]

{ #category : #tests }
FileReferenceAttributeTests >> testIsCharacter [

	self deny: self tempFileResource file isCharacter.
	self assert: self tempFileResource file entry isCharacter equals: self tempFileResource file isCharacter.

]

{ #category : #tests }
FileReferenceAttributeTests >> testIsDirectory [

	self assert: self tempFileResource file isDirectory equals: false.
	self assert: self tempFileResource file entry isDirectory equals: self tempFileResource file isDirectory.
	self assert: FileLocator temp isDirectory.
]

{ #category : #tests }
FileReferenceAttributeTests >> testIsExecutable [

	self assert: FileLocator vmBinary isExecutable.
	self assert: FileLocator vmBinary entry isExecutable equals: FileLocator vmBinary isExecutable
]

{ #category : #tests }
FileReferenceAttributeTests >> testIsFIFO [

	self assert: self tempFileResource file isFIFO equals: false.
	self assert: self tempFileResource file entry isFIFO equals: self tempFileResource file isFIFO.
]

{ #category : #tests }
FileReferenceAttributeTests >> testIsFile [

	self assert: self tempFileResource file isFile.
	self assert: self tempFileResource file entry isFile equals: self tempFileResource file isFile.
	self deny: FileLocator temp isFile.
]

{ #category : #tests }
FileReferenceAttributeTests >> testIsReadable [

	self assert: self tempFileResource file isReadable.
	self assert: self tempFileResource file entry isReadable equals: self tempFileResource file isReadable.
]

{ #category : #tests }
FileReferenceAttributeTests >> testIsRegular [

	self assert: self tempFileResource file isRegular.
	self assert: self tempFileResource file entry isRegular equals: self tempFileResource file isRegular
]

{ #category : #tests }
FileReferenceAttributeTests >> testIsSocket [

	self assert: self tempFileResource file isSocket equals: false.
	self assert: self tempFileResource file entry isSocket equals: self tempFileResource file isSocket.
]

{ #category : #tests }
FileReferenceAttributeTests >> testIsSymlink [

	self assert: self tempFileResource file isSymlink equals: false.
	self assert: self tempFileResource file entry isSymlink equals: self tempFileResource file isSymlink.
]

{ #category : #tests }
FileReferenceAttributeTests >> testIsWriteable [

	self assert: self tempFileResource file isWritable.
	self assert: self tempFileResource file entry isWritable equals: self tempFileResource file isWritable.
]

{ #category : #tests }
FileReferenceAttributeTests >> testModificationTime [

	| resource modificationTime |
	resource := self tempFileResource.
	modificationTime := resource file modificationTime.
	self assert: resource beforeCreationTime <= modificationTime.
	self assert: resource afterCreationTime >= modificationTime.
	self assert: resource file entry modificationTime equals: modificationTime.

]

{ #category : #tests }
FileReferenceAttributeTests >> testNLink [
	"Assume that there is only one hard link to the just created temporary file"

	self assert: self tempFileResource file numberOfHardLinks equals: 1.
	self assert: self tempFileResource file entry numberOfHardLinks equals: self tempFileResource file numberOfHardLinks.
]

{ #category : #tests }
FileReferenceAttributeTests >> testTargetFile [

	"The temporary file isn't a symbolic link, so the targetFile isn't supported"
	self should: [ self tempFileResource file targetPath ] raise: FileAttributeNotSupported.
	self should: [ self tempFileResource file entry targetPath ] raise: FileAttributeNotSupported.
]
