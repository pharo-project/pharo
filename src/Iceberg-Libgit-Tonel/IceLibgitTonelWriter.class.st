"
I write versions and snapshots in tonel format.
"
Class {
	#name : #IceLibgitTonelWriter,
	#superclass : #MCWriter,
	#traits : 'TIceRepositoryWriter',
	#classTraits : 'TIceRepositoryWriter classTrait',
	#instVars : [
		'repositoryDirectory'
	],
	#category : #'Iceberg-Libgit-Tonel'
}

{ #category : #accessing }
IceLibgitTonelWriter class >> description [
	^ 'Tonel'
]

{ #category : #'accessing - ancient preference selectors' }
IceLibgitTonelWriter class >> directoryNameFor: aPackage [
	^ aPackage name
]

{ #category : #writing }
IceLibgitTonelWriter class >> fileOut: aVersion on: dir [
	(self on: dir) writeVersion: aVersion

]

{ #category : #accessing }
IceLibgitTonelWriter class >> forInternalStore [
	^ TonelWriter
]

{ #category : #writing }
IceLibgitTonelWriter class >> forInternalStoreFileOut: aMCVersion on: aRepository [
	self forInternalStore 
		fileOut: aMCVersion
		on: aRepository subdirectoryReference
]

{ #category : #accessing }
IceLibgitTonelWriter class >> id [
	^ #tonel
]

{ #category : #testing }
IceLibgitTonelWriter class >> isDefault [
	^ true
]

{ #category : #testing }
IceLibgitTonelWriter class >> isValidPackage: aFileReference [ 
	
	^ aFileReference basename first isLetter
		and: [ aFileReference isDirectory
		and: [ (aFileReference / 'package.st') exists ] ]
]

{ #category : #accessing }
IceLibgitTonelWriter class >> logClass [
	^ IceLibgitTonelLog
]

{ #category : #'instance creation' }
IceLibgitTonelWriter class >> on: dir [
	^ self new repositoryDirectory: dir
	
	

]

{ #category : #accessing }
IceLibgitTonelWriter class >> packageNameFromDirectory: aFileReference [
	^ aFileReference basename
]

{ #category : #accessing }
IceLibgitTonelWriter class >> readerClass [
	^ IceLibgitTonelReader
]

{ #category : #accessing }
IceLibgitTonelWriter >> repositoryDirectory [
	^ repositoryDirectory
]

{ #category : #accessing }
IceLibgitTonelWriter >> repositoryDirectory: anObject [
	repositoryDirectory := anObject
]

{ #category : #writing }
IceLibgitTonelWriter >> writeSnapshot: aSnapshot [
	(TonelWriter on: self repositoryDirectory) writeSnapshot: aSnapshot
		
]

{ #category : #writing }
IceLibgitTonelWriter >> writeVersion: aVersion [
	(self repositoryDirectory / aVersion package name) ensureDeleteAll.
	self writeSnapshot: aVersion snapshot
	
]
