Class {
	#name : 'FSSymlinkTest',
	#superclass : 'TestCase',
	#category : 'FileSystem-PlatformSpecific-Tests',
	#package : 'FileSystem-PlatformSpecific-Tests'
}

{ #category : 'tests' }
FSSymlinkTest >> testDeleteSymbolicLink [
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

{ #category : 'tests' }
FSSymlinkTest >> testSymbolicLink [
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

{ #category : 'tests' }
FSSymlinkTest >> testSymbolicLinkInMemory [
	"Confirm that in-memory files behave as expected with symbolic link requests.
	That is... symbolic links aren't supported, to #isSymlink always returns false and the targetFileReference is always the original file"
	| file |

	file := FileSystem memory root / 'a.file'.
	file writeStreamDo: [ :s | s << 'hello, world' ].

	self deny: file isSymlink.
	self assert: file targetFileReference equals: file.
]

{ #category : 'tests' }
FSSymlinkTest >> testSymbolicLinkTargetPath [
	
	| tempDir dDir symbolicLink symlinkCommand status |

	OSPlatform current isWindows ifTrue: [ ^ self ].

	[ tempDir := FileReference newTempFilePrefix: 'testSymbolLinkTargetPath' suffix: 'dir'.
	tempDir ensureCreateDirectory.
	dDir := tempDir / 'a/b/c/d'.
	dDir ensureCreateDirectory.
	symbolicLink := tempDir / 'a/sl'.

	symlinkCommand := 'cd ', (tempDir / 'a') fullName, ' && ln -s ', dDir fullName, ' sl'.
	status := LibC uniqueInstance system: symlinkCommand.
	status = 0 ifFalse: [ self error: 'Unable to create symbolic link' ].

	self assert: symbolicLink isSymlink.
	self assert: (tempDir / 'a/sl/../abc') canonicalize path
		equals: (tempDir / 'a/b/c/abc') path.

	"Symbolic link operations shouldn't raise an error if the target doesn't exist"
	dDir deleteAll.
	self assert: symbolicLink isSymlink.
	self assert: (tempDir / 'a/sl/../abc') canonicalize path
		equals: (tempDir / 'a/b/c/abc') path ]
			ensure: [ tempDir ensureDeleteAll ].

]
