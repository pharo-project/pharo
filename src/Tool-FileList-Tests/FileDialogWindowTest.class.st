"
Unit tests for File dialog window
"
Class {
	#name : #FileDialogWindowTest,
	#superclass : #TestCase,
	#instVars : [
		'memFs'
	],
	#category : #'Tool-FileList-Tests'
}

{ #category : #'tests - helper' }
FileDialogWindowTest >> dialogOnMemFs [
	^ FileDialogWindow onFileSystem: memFs
]

{ #category : #'tests - helper' }
FileDialogWindowTest >> randomTestPathName [

	^ 'FileDialogWindowTest-' , UUIDGenerator next asString
]

{ #category : #running }
FileDialogWindowTest >> setUp [
	super setUp.

	memFs := FileSystem memory
]

{ #category : #tests }
FileDialogWindowTest >> testDeleteFileOrDirectory [
	| pathName aFolder dialog |

	pathName := self randomTestPathName.
	aFolder := (memFs workingDirectory / pathName) ensureCreateDirectory.

	"Make sure that aFolder is not empty"
	aFolder / 'make-directory-non-empty-issue-6331.txt' writeStreamDo: [ :stream | stream nextPutAll: 'foobar' ].

	"Select aFolder on the left side"
	dialog := self dialogOnMemFs selectPath: '/'; yourself.

	"Check that it is selected"
	self assert: dialog selectedFileDirectory fullName equals: '/'.

	"Check that there is only one directory in the selected directory, and that it is the one we created"
	self assert: dialog files size equals: 1.
	self assert: dialog files first fullName asLowercase equals: aFolder fullName asLowercase.

	"Select that one directory on the right side"
	dialog selectedFileIndex: 1.

	"Check that the selected directory is the one we created"
	self assert: dialog selectedFileName asLowercase equals: aFolder basename asLowercase.

	"Delete the selected directory"
	dialog deleteDirectoryEntry: dialog selectedFileEntry.

	"Check that it has indeed been deleted"
	self deny: aFolder exists
]

{ #category : #tests }
FileDialogWindowTest >> testUsingMemoryFileSystemAbsolutePathCanBeSelected [
	| pathName aFolder dialog |

	pathName := self randomTestPathName.
	aFolder := (memFs workingDirectory / pathName) ensureCreateDirectory.

	dialog := self dialogOnMemFs selectPath: aFolder; yourself.

	"just compare the lower-case path names for now since on mac the test will fail under certain circumstances..."
	self
		assert: dialog selectedFileDirectory fullName asLowercase
		equals: aFolder fullName asLowercase
]

{ #category : #tests }
FileDialogWindowTest >> testUsingMemoryFileSystemRelativePathCanBeSelected [
	| pathName aFolder dialog |

	pathName := self randomTestPathName.
	aFolder := (memFs workingDirectory / pathName) ensureCreateDirectory.

	dialog := self dialogOnMemFs selectPath: (memFs referenceTo: pathName); yourself.

	"just compare the lower-case path names for now since on mac the test will fail under certain circumstances..."
	self
		assert: dialog selectedFileDirectory fullName asLowercase
		equals: aFolder fullName asLowercase
]

{ #category : #tests }
FileDialogWindowTest >> testUsingMemoryFileSystemSelectInvalidDirectory [
	"Tests that selecting a non existent directory defaults to selecting the working directory"
	| nonExistentDirectory dialog |
	nonExistentDirectory := 'cant-possibly-exist-for-testIssue6406'.
	dialog := self dialogOnMemFs selectPath: nonExistentDirectory; yourself.

	"just compare the lower-case path names for now since on mac the test will fail under certain circumstances..."
	self
		assert: dialog selectedFileDirectory fullName asLowercase
		equals: memFs workingDirectory fullName asLowercase
]
