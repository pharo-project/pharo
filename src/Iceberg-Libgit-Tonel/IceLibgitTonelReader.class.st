"
I'm a reader for the tonel file format.

"
Class {
	#name : #IceLibgitTonelReader,
	#superclass : #TonelReader,
	#traits : 'TIceRepositoryReader',
	#classTraits : 'TIceRepositoryReader classTrait',
	#instVars : [
		'iceVersion'
	],
	#category : #'Iceberg-Libgit-Tonel'
}

{ #category : #accessing }
IceLibgitTonelReader class >> id [
	^ #tonel
]

{ #category : #'instance creation' }
IceLibgitTonelReader class >> on: aCommitOrTree iceVersion: iceVersion [
	^ (self on: aCommitOrTree) 
		iceVersion: iceVersion; 
		yourself
]

{ #category : #accessing }
IceLibgitTonelReader >> commit [
	^ stream
]

{ #category : #accessing }
IceLibgitTonelReader >> fileUtils [
	^ IceLibgitTonelFileUtils
]

{ #category : #accessing }
IceLibgitTonelReader >> iceVersion [
	^ iceVersion
]

{ #category : #accessing }
IceLibgitTonelReader >> iceVersion: anObject [
	| repository packageDirectoryName |
	iceVersion := anObject.
	repository := iceVersion commit repository.
	package := iceVersion package.
	
	packageDirectoryName := iceVersion commit writerClass directoryNameFor: package.
	self packageDirectory: (repository subdirectoryPath / packageDirectoryName) pathString
]

{ #category : #loading }
IceLibgitTonelReader >> loadVersionInfo [
	^ info := self iceVersion info
]

{ #category : #accessing }
IceLibgitTonelReader >> package [
	^ MCPackage named: self iceVersion packageName
]

{ #category : #private }
IceLibgitTonelReader >> packageNameFromPackageDirectory [
	^ iceVersion packageName
]

{ #category : #accessing }
IceLibgitTonelReader >> repository [
	^ self iceVersion repository
]

{ #category : #accessing }
IceLibgitTonelReader >> versionName [
	^ ('{1}-{2}.{3}' format: {
		self packageNameFromPackageDirectory. 
		self commit compatibleUsername. 
		self versionNumber
	})
]
