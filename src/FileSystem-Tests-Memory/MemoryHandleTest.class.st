"
SUnit tests for memory handles, the tests may be found in superclass
"
Class {
	#name : 'MemoryHandleTest',
	#superclass : 'FileSystemHandleTest',
	#category : 'FileSystem-Tests-Memory-Base',
	#package : 'FileSystem-Tests-Memory',
	#tag : 'Base'
}

{ #category : 'running' }
MemoryHandleTest >> createFileSystem [
	^ FileSystem memory
]
