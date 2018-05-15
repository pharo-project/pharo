"
SUnit tests for MemoryFileSystem
"
Class {
	#name : #MemoryFileSystemTest,
	#superclass : #FileSystemTest,
	#category : #'FileSystem-Tests-Memory-Base'
}

{ #category : #'initialize-release' }
MemoryFileSystemTest >> createFileSystem [
	^ FileSystem memory
]

{ #category : #tests }
MemoryFileSystemTest >> testEqual [
	| other |
	other := self createFileSystem.
	self deny: filesystem = other
]

{ #category : #tests }
MemoryFileSystemTest >> testIsMemoryFileSystem [
	self assert: filesystem isMemoryFileSystem.
	
]
