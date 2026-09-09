Class {
	#name : 'MemoryFileManipulationTest',
	#superclass : 'FileManipulationTest',
	#category : 'FileSystem-Memory-Tests-Base',
	#package : 'FileSystem-Memory-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
MemoryFileManipulationTest >> root [

	^ FileSystem memory
]

{ #category : 'tests' }
MemoryFileManipulationTest >> testModifiedTimeWhenFileModifiedByWriteStream [
	"I should be in my superclass but the disk FS has a bug for now."

	| modifiedTime fileReference |
	fileReference := (firstFolder / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	fileReference writeStreamDo: [ :aStream | aStream nextPutAll: 'data' ].
	self assert: modifiedTime isNotNil.
	self deny: modifiedTime equals: fileReference modificationTime
]

{ #category : 'tests' }
MemoryFileManipulationTest >> testModifiedTimeWhenFileModifiedWithBinaryWriteStream [
	"I should be in my superclass but the disk FS has a bug for now."

	| modifiedTime fileReference data |
	fileReference := (firstFolder / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	data := 'some data'.
	fileReference binaryWriteStreamDo: [ :aStream | aStream nextPutAll: data asByteArray ].
	self assert: modifiedTime isNotNil.
	self deny: modifiedTime equals: fileReference modificationTime.
	self assert: data asByteArray equals: (fileReference binaryReadStreamDo: [ :aStream | aStream upToEnd ]).
	self assert: data equals: (fileReference readStreamDo: [ :aStream | aStream upToEnd ])
]

{ #category : 'tests' }
MemoryFileManipulationTest >> testModifiedTimeWhenFileWrittenTo [
	"I should be in my superclass but the disk FS has a bug for now."

	| modifiedTime fileReference |
	fileReference := (firstFolder / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	fileReference binaryWriteStreamDo: [ :aStream | aStream nextPutAll: 'data' asByteArray ].
	self assert: modifiedTime isNotNil.
	self deny: modifiedTime equals: fileReference modificationTime
]

{ #category : 'tests' }
MemoryFileManipulationTest >> testModifiedTimeWhenHandleTruncated [
	"I should be in my superclass but the disk FS has a bug for now."

	| modifiedTime fileReference handle |
	fileReference := (firstFolder / 'file.txt') ensureCreateFile.
	handle := fileReference openWritable: true.
	modifiedTime := self lastModificationTimeOf: fileReference.
	handle truncate.
	self assert: modifiedTime isNotNil.
	self deny: modifiedTime equals: fileReference modificationTime
]
