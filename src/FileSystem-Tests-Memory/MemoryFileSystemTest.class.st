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

{ #category : #tests }
MemoryFileSystemTest >> testModifiedTimeWhenFileCreated [
	self assert: (filesystem / 'file.txt') ensureCreateFile modificationTime notNil
]

{ #category : #tests }
MemoryFileSystemTest >> testModifiedTimeWhenFileModifiedByWriteStream [
	| modifiedTime fileReference |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := fileReference modificationTime.
	fileReference writeStreamDo: [ :aStream | aStream nextPutAll: 'data' ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
]

{ #category : #tests }
MemoryFileSystemTest >> testModifiedTimeWhenFileModifiedWithBinaryWriteStream [
	| modifiedTime fileReference data |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := fileReference modificationTime.
	data := 'some data'.
	fileReference binaryWriteStreamDo: [ :aStream | aStream nextPutAll: data ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime.
	self
		assert: data asByteArray
		equals: (fileReference binaryReadStreamDo: [ :aStream | aStream upToEnd ]).
	self assert: data equals: (fileReference readStreamDo: [ :aStream | aStream upToEnd ])
]

{ #category : #tests }
MemoryFileSystemTest >> testModifiedTimeWhenFileWrittenTo [
	| modifiedTime fileReference |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := fileReference modificationTime.
	fileReference binaryWriteStreamDo: [ :aStream | aStream nextPutAll: 'data' ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
]

{ #category : #tests }
MemoryFileSystemTest >> testModifiedTimeWhenHandleTruncated [
	| modifiedTime fileReference handle |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	handle := fileReference openWritable: true.
	modifiedTime := fileReference modificationTime.
	handle truncate.
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
]
