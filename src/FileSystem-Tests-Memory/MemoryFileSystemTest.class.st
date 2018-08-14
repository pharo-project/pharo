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
MemoryFileSystemTest >> testCurrentEqual [
	| other another |
	another := FileSystem currentMemoryFileSystem.
	other := FileSystem currentMemoryFileSystem.
	self assert: other equals: another
]

{ #category : #tests }
MemoryFileSystemTest >> testIsMemoryFileSystem [
	self assert: filesystem isMemoryFileSystem.
	
]

{ #category : #tests }
MemoryFileSystemTest >> testNotEqualWhenCreatingNewMemoryFileSystem [
	| other |
	other := FileSystem memory.
	self deny: other equals: filesystem
]

{ #category : #tests }
MemoryFileSystemTest >> testNotEqualWhenRequestingMemoryFileSystem [
	| other |
	other := self createFileSystem.
	self deny: other equals: filesystem
]
