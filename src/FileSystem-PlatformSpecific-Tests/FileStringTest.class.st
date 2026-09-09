Class {
	#name : 'FileStringTest',
	#superclass : 'TestCase',
	#category : 'FileSystem-PlatformSpecific-Tests',
	#package : 'FileSystem-PlatformSpecific-Tests'
}

{ #category : 'tests' }
FileStringTest >> testAsFileLocatorOrReferenceReturnsALocatorWhenStringRefersToAKnownOrigin [
	| pathString locator |
	pathString := (FileLocator home / 'foo') asFileReference.

	locator := pathString asFileLocatorOrReference.

	self
		assert: locator origin
		equals: #home.
	self
		assert: locator fullName
		equals: pathString fullName
]

{ #category : 'tests' }
FileStringTest >> testAsFileLocatorOrReferenceReturnsAReferenceWhenStringDoesNotReferToAKnownOrigin [
	| fileRef result |
	fileRef := '/foo' asFileReference.

	result := fileRef fullName asFileLocatorOrReference.

	self
		assert: result
		equals: fileRef
]
