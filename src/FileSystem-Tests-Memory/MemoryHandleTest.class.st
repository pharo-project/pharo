"
SUnit tests for memory handles, the tests may be found in superclass
"
Class {
	#name : #MemoryHandleTest,
	#superclass : #FileSystemHandleTest,
	#category : #'FileSystem-Tests-Memory-Base'
}

{ #category : #running }
MemoryHandleTest >> createFileSystem [
	^ FileSystem memory
]
