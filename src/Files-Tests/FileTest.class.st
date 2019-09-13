"
Unit tests for file operations
"
Class {
	#name : #FileTest,
	#superclass : #TestCase,
	#category : #'Files-Tests-Core'
}

{ #category : #running }
FileTest >> tearDown [

	'asd.txt' asFileReference ensureDelete.
	super tearDown
]

{ #category : #tests }
FileTest >> testCheckExistenceOfExistingFileThrowsException [

	| file |
	file := File named: 'asd.txt'.
	file writeStream close.
	self should: [file checkDoesNotExist] raise: FileAlreadyExistsException.
]

{ #category : #tests }
FileTest >> testCheckExistenceOfNonExistingFileDoesNotThrowException [

	| file |
	file := File named: 'asd.txt'.
	[file checkDoesNotExist] 
		on: FileAlreadyExistsException
		do: [ :ex | self assert: false description: 'File should not exist!' ]
]

{ #category : #tests }
FileTest >> testFileDelete [

	| file |
	file := File named: 'asd.txt'.
	file writeStream close.
	file delete.
	self deny: file exists.
]

{ #category : #tests }
FileTest >> testFileExists [

	| file |
	file := File named: 'asd.txt'.
	file writeStream close.
	self assert: file exists.
	file delete.
]

{ #category : #tests }
FileTest >> testOpenFileForReadDoesNotDeleteExistingFile [

	| size |
	(File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].

	size := (File named: 'asd.txt') size.
	self assert: size equals: 3.
]

{ #category : #tests }
FileTest >> testOpeningFileNormallyDoesNotDeleteExistingFile [

	| size |
	(File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].

	size := (File named: 'asd.txt') size.
	self assert: size equals: 3.
]

{ #category : #tests }
FileTest >> testOpeningFileObjectCreatesFile [

	(File named: 'asd.txt') writeStream close.
	"Our File implementation should not cache anything.
	This test is based on this assumption."
	self assert: (File named: 'asd.txt') exists.
]

{ #category : #tests }
FileTest >> testOpeningFileSetsPositionAtBeginning [

	| file |
	(File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].
	file := (File named: 'asd.txt') openForWrite.
	[self assert: file position equals: 0 
		] ensure: [ file ifNotNil: [ file close ] ]
]

{ #category : #tests }
FileTest >> testOpeningForAppendSetsPositionAtEnd [

	| file |
	(File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].
	file := (File named: 'asd.txt') openForAppend.
	[ self assert: file position equals: 3
		] ensure: [ file ifNotNil: [ file close ] ]
]
