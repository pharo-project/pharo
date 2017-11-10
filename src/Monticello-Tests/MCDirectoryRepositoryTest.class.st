Class {
	#name : #MCDirectoryRepositoryTest,
	#superclass : #MCRepositoryTest,
	#instVars : [
		'directory'
	],
	#category : #Monticello-Tests
}

{ #category : #actions }
MCDirectoryRepositoryTest >> addVersion: aVersion [
	| file |
	file := (directory / aVersion fileName) asFileReference writeStream.
	aVersion fileOutOn: file.
	file close.
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
	repository := MCDirectoryRepository new directory: self directory
]

{ #category : #running }
MCDirectoryRepositoryTest >> tearDown [
	self directory ensureDeleteAll.
	
]
