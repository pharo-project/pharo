"
Try and test file attribute access from FileReference.

As Pharo doesn't provide a mechanism to set the attributes, pick a few well known files and make sure they have reasonable attributes, e.g. the VM is executable, it isn't older than when the classes were first created or newer than today, etc.

"
Class {
	#name : #FileReferenceAttributeTests,
	#superclass : #TestCase,
	#category : #'FileSystem-Tests-Attributes'
}

{ #category : #resources }
FileReferenceAttributeTests class >> resources [

	^Array with: DiskFileAttributesTestsResources.
]

{ #category : #'helper methods' }
FileReferenceAttributeTests >> tempFileResource [

	^DiskFileAttributesTestsResources current
]

{ #category : #tests }
FileReferenceAttributeTests >> testCreationTime [

	| resource creationTime |
	resource := self tempFileResource.
	creationTime := resource file creationTime.
	self assert: resource beforeCreationTime <= creationTime.
	self assert: resource afterCreationTime >= creationTime.

]

{ #category : #tests }
FileReferenceAttributeTests >> testExists [

	self assert: self tempFileResource file exists
]

{ #category : #tests }
FileReferenceAttributeTests >> testFileSize [

	self assert: self tempFileResource file size equals: 72.

]

{ #category : #tests }
FileReferenceAttributeTests >> testIsBlock [

	self deny: self tempFileResource file isBlock

]

{ #category : #tests }
FileReferenceAttributeTests >> testIsCharacter [

	self deny: self tempFileResource file isCharacter

]

{ #category : #tests }
FileReferenceAttributeTests >> testIsDirectory [

	self deny: self tempFileResource file isDirectory.
	self assert: FileLocator temp isDirectory.
]

{ #category : #tests }
FileReferenceAttributeTests >> testIsFile [

	self assert: self tempFileResource file isFile.
	self deny: FileLocator temp isFile.
]

{ #category : #tests }
FileReferenceAttributeTests >> testIsReadable [

	self assert: self tempFileResource file isReadable.

]

{ #category : #tests }
FileReferenceAttributeTests >> testIsSymlink [

	self deny: self tempFileResource file isSymlink.

]

{ #category : #tests }
FileReferenceAttributeTests >> testIsWriteable [

	self assert: self tempFileResource file isWritable.

]

{ #category : #tests }
FileReferenceAttributeTests >> testModificationTime [

	| resource modificationTime |
	resource := self tempFileResource.
	modificationTime := resource file modificationTime.
	self assert: resource beforeCreationTime <= modificationTime.
	self assert: resource afterCreationTime >= modificationTime.

]
