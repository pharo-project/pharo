"
SUnit tests for memory handles, the tests may be found in superclass
"
Class {
	#name : 'MemoryHandleTest',
	#superclass : 'FileSystemHandleTest',
	#category : 'FileSystem-Memory-Tests-Base',
	#package : 'FileSystem-Memory-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
MemoryHandleTest >> createFileSystem [
	^ FileSystem memory
]
