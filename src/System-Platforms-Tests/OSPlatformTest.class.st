Class {
	#name : 'OSPlatformTest',
	#superclass : 'TestCase',
	#category : 'System-Platforms-Tests-Base',
	#package : 'System-Platforms-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
OSPlatformTest >> testAPI [
	"A test to make sure the full API will work"
	OSPlatform current
		isMacOS;
		isMacOSX;
		isWin32;
		isWin64;
		isWindows;
		isUnix;
		isLinux;
		isX11;
		name;
		subtype;
		family
]

{ #category : 'tests' }
OSPlatformTest >> testCurrentWorkingDirectory [
	"This is a test to make sure getCurrentWorkingDirectory is working"

 self assert: FileSystem workingDirectory fullName equals:  OSPlatform current currentWorkingDirectoryPath.
 self assert: 'relativePath' asFileReference asAbsolute parent fullName equals: OSPlatform current currentWorkingDirectoryPath
]

{ #category : 'tests' }
OSPlatformTest >> testCurrentWorkingDirectoryWithBufferSize [
	"This is a test to make sure getCurrentWorkingDirectoryWithBufferSize is working"

 self assert: FileSystem workingDirectory fullName equals:  (OSPlatform current currentWorkingDirectoryPathWithBufferSize: 1024) .
 self assert: 'relativePath' asFileReference asAbsolute parent fullName equals: (OSPlatform current currentWorkingDirectoryPathWithBufferSize: 1024)
]

{ #category : 'tests' }
OSPlatformTest >> testNewFileInWorkingDirectory [
	"This is a test to make sure new files are created in the current working directory"

	| file |
	file := File named: 'asd.txt'.
	file writeStream close.
	self
		assert: file name asPath parent
		equals: OSPlatform current currentWorkingDirectoryPath asPath.
	file delete
]
