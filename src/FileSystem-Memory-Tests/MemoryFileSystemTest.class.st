"
SUnit tests for MemoryFileSystem
"
Class {
	#name : 'MemoryFileSystemTest',
	#superclass : 'FileSystemTest',
	#category : 'FileSystem-Memory-Tests-Base',
	#package : 'FileSystem-Memory-Tests',
	#tag : 'Base'
}

{ #category : 'initialization' }
MemoryFileSystemTest >> createFileSystem [
	^ FileSystem memory
]

{ #category : 'tests' }
MemoryFileSystemTest >> lastModificationTimeOf: fileReference [
	"DateAndTime primUTCMicrosecondsClock is not precise across all OS so put in slight delay between calling modificationTime"

	^ [ fileReference modificationTime ]
		ensure: [ (Delay forMilliseconds: 100) wait ]
]

{ #category : 'tests' }
MemoryFileSystemTest >> testCurrentEqual [

	| other another |
	another := FileSystem currentMemoryFileSystem.
	other := FileSystem currentMemoryFileSystem.
	self assert: other equals: another
]

{ #category : 'tests' }
MemoryFileSystemTest >> testIsMemoryFileSystem [
	self assert: filesystem isMemoryFileSystem
]

{ #category : 'tests' }
MemoryFileSystemTest >> testNotEqualWhenCreatingNewMemoryFileSystem [
	| other |
	other := FileSystem memory.
	self deny: other equals: filesystem
]

{ #category : 'tests' }
MemoryFileSystemTest >> testNotEqualWhenRequestingMemoryFileSystem [
	| other |
	other := self createFileSystem.
	self deny: other equals: filesystem
]
