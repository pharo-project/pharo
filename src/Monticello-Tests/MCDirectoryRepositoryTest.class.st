Class {
	#name : #MCDirectoryRepositoryTest,
	#superclass : #MCRepositoryTest,
	#instVars : [
		'directory'
	],
	#category : #'Monticello-Tests-Repository'
}

{ #category : #actions }
MCDirectoryRepositoryTest >> addVersion: aVersion [
	
	(directory / aVersion fileName) asFileReference
		binaryWriteStreamDo: [ :stream |
			aVersion fileOutOn: stream ]
]

{ #category : #accessing }
MCDirectoryRepositoryTest >> directory [
	directory ifNil:
		[directory := 'mctest' asFileReference.
		directory ensureCreateDirectory].
	^ directory
]

{ #category : #running }
MCDirectoryRepositoryTest >> setUp [
	super setUp.
	repository := MCDirectoryRepository new directory: self directory
]

{ #category : #running }
MCDirectoryRepositoryTest >> tearDown [

	self directory ensureDeleteAll.
	super tearDown
]
