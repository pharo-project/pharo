"
SUnit tests for class DeleteVisitor
"
Class {
	#name : 'DeleteVisitorTest',
	#superclass : 'SingleTreeTest',
	#category : 'FileSystem-Core-Tests-Visitors',
	#package : 'FileSystem-Core-Tests',
	#tag : 'Visitors'
}

{ #category : 'tests' }
DeleteVisitorTest >> testBeta [
	self setUpGreek.
	DeleteVisitor delete: (filesystem / 'alpha' / 'beta').
	self assert: (filesystem isDirectory: '/alpha').
	self assert: (filesystem isDirectory: '/alpha/epsilon').
	self deny: (filesystem exists: '/alpha/beta')
]

{ #category : 'tests' }
DeleteVisitorTest >> testSymbolicLink [
	"The delete visitor should not follow symbolic links and should delete them as a file."
	| tempDir aDir bDir symbolicLink symlinkCommand status |

	OSPlatform current isWindows ifTrue: [ ^ self ].

	[ tempDir := FileReference newTempFilePrefix: 'testSymbolLinkTargetPath' suffix: 'dir'.
	self deny: tempDir exists.
	tempDir ensureCreateDirectory.
	aDir := tempDir / 'a'.
	bDir := tempDir / 'a/b'.
	(bDir / 'c/d') ensureCreateDirectory.
	symbolicLink := bDir / 'c/sl'.

	symlinkCommand := 'cd ', (bDir / 'c') fullName, ' && ln -s ', aDir fullName, ' sl'.
	status := LibC uniqueInstance system: symlinkCommand.
	status = 0 ifFalse: [ self error: 'Unable to create symbolic link' ].

	self assert: symbolicLink isSymlink.
	self assert: symbolicLink isDirectory.
	self assert: symbolicLink targetFileReference equals: aDir.
	bDir ensureDeleteAll.
	self assert: aDir exists.
	self deny: bDir exists ]
			ensure: [ tempDir ensureDeleteAll ].

]
