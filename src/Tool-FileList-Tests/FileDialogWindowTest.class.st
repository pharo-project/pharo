"
Unit tests for File dialog window
"
Class {
	#name : #FileDialogWindowTest,
	#superclass : #TestCase,
	#category : #'Tool-FileList-Tests'
}

{ #category : #tests }
FileDialogWindowTest >> testIssue6406 [
	| aFolder pathName |
	pathName := 'FileDialogWindowTest-' , UUIDGenerator next asString.
	aFolder := (FileSystem workingDirectory / pathName) ensureCreateDirectory.
	[ | invalidFolder dialog workingFolder |
	dialog := FileDialogWindow new selectPathName: aFolder fullName.
	"just compare the lower-case path names for now since on mac the test will fail under certain circumstances..."
	self assert: dialog selectedFileDirectory fullName asLowercase = aFolder fullName asLowercase description: 'Expected Absolute folder paths to work'.
	dialog := FileDialogWindow new selectPathName: pathName asFileReference fullName.
	self assert: dialog selectedFileDirectory fullName asLowercase = aFolder fullName asLowercase description: 'Expected Relative folder paths to work'.
	invalidFolder := 'cant-possibly-exist-for-testIssue6406'.
	dialog := FileDialogWindow new selectPathName: invalidFolder.
	workingFolder := FileSystem workingDirectory fullName.
	self assert: dialog selectedFileDirectory fullName asLowercase = workingFolder asLowercase description: 'Expected Invalid folder to default to the working folder' ]
		ensure: [ aFolder ensureDelete ]
]

{ #category : #tests }
FileDialogWindowTest >> testUsingMemoryFileSystem [
	| pathName fs aFolder |
	fs := FileSystem memory.
	pathName := 'FileDialogWindowTest-' , UUIDGenerator next asString.
	aFolder := (fs workingDirectory / pathName) ensureCreateDirectory.
	[ | invalidFolder dialog workingFolder |
	dialog := (FileDialogWindow onFileSystem: fs) selectPathName: aFolder fullName.
	"just compare the lower-case path names for now since on mac the test will fail under certain circumstances..."
	self assert: dialog selectedFileDirectory fullName asLowercase = aFolder fullName asLowercase description: 'Expected Absolute folder paths to work'.
	dialog := (FileDialogWindow onFileSystem: fs) selectPathName: (fs referenceTo: pathName) fullName.
	self assert: dialog selectedFileDirectory fullName asLowercase = aFolder fullName asLowercase description: 'Expected Relative folder paths to work'.
	invalidFolder := 'cant-possibly-exist-for-testIssue6406'.
	dialog := (FileDialogWindow onFileSystem: fs) selectPathName: invalidFolder.
	workingFolder := fs workingDirectory fullName.
	self assert: dialog selectedFileDirectory fullName asLowercase = workingFolder asLowercase description: 'Expected Invalid folder to default to the working folder' ]
		ensure: [ aFolder ensureDelete ]
]
