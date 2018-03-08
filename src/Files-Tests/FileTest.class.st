Class {
	#name : #FileTest,
	#superclass : #TestCase,
	#category : #'Files-Tests'
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
	self shouldnt: [file checkDoesNotExist] raise: Error.
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
FileTest >> testFilesAreRegisteredInWeakRegistry [

	|  f |
	f := (File named: 'asd.txt') writeStream.
	self assert: (File registry keys includes: f)
]

{ #category : #tests }
FileTest >> testOpenFileForReadDoesNotDeleteExistingFile [

	| file |
	file := (File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].

	file := (File named: 'asd.txt') readStream.
	self assert: file size equals: 3.
]

{ #category : #tests }
FileTest >> testOpeningFileNormallyDoesNotDeleteExistingFile [

	| file |
	file := (File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].

	file := (File named: 'asd.txt') readStream.
	self assert: file size equals: 3.
]

{ #category : #tests }
FileTest >> testOpeningFileObjectCreatesFile [

	(File named: 'asd.txt') writeStream.
	"Our File implementation should not cache anything.
	This test is based on this assumption."
	self assert: (File named: 'asd.txt') exists.
]

{ #category : #tests }
FileTest >> testOpeningFileSetsPositionAtBeggining [

	| file |
	(File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].
	file := (File named: 'asd.txt') openForWrite.
	self assert: file position equals: 0.
]

{ #category : #tests }
FileTest >> testOpeningForAppendSetsPositionAtEnd [

	| file |
	(File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].
	file := (File named: 'asd.txt') openForAppend.
	self assert: file position equals: 3.
]

{ #category : #tests }
FileTest >> testRegistryShouldBeCleaned [

	| oldSize |
	"We force some garbage collection to avoid noise in the test"
	3 timesRepeat: [Smalltalk garbageCollect].
	oldSize := File registry size.

	(File named: 'asd.txt') writeStream.
	3 timesRepeat: [Smalltalk garbageCollect].
	
	self assert: oldSize equals: File registry size.
]
