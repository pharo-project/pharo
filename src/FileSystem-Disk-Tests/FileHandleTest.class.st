"
SUnit tests for file handles, the tests may be found in superclass
"
Class {
	#name : 'FileHandleTest',
	#superclass : 'FileSystemHandleTest',
	#category : 'FileSystem-Disk-Tests-Base',
	#package : 'FileSystem-Disk-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
FileHandleTest >> createFileSystem [
	^ FileSystem store: DiskStore activeClass createDefault
]
