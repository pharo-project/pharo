"
I am a node in a tree of Epicea logs that represents a log file. Read more in my superclass comment.
"
Class {
	#name : #EpFileLogNode,
	#superclass : #EpLogNode,
	#instVars : [
		'globalName',
		'directoryEntry',
		'log'
	],
	#category : 'EpiceaBrowsers-UI-Graph'
}

{ #category : #convenience }
EpFileLogNode class >> fromAllLogsIn: directory [
	
	| nodes |
	nodes := directory entries 
		select: [ :each | each basename endsWith: OmFileStore defaultFileSuffix ]
		thenCollect: [ :entry | self withEntry: entry ].
		
	^ nodes
]

{ #category : #'instance creation' }
EpFileLogNode class >> withEntry: aDirectoryEntry [

	^ self basicNew 
		initializeWithEntry: aDirectoryEntry;
		yourself 
]

{ #category : #converting }
EpFileLogNode >> asMorph [

	| morphFactory |
	morphFactory := EpMorphFactory default.
	
	^ morphFactory rowWithAll: {
		morphFactory emphasizedStringFor: self globalName.
		morphFactory smallGap.
		morphFactory secondaryStringFor: self modificationTime asDate epiceaBrowsersAsString.
	}
]

{ #category : #accessing }
EpFileLogNode >> fileReference [

	^ directoryEntry asFileReference
]

{ #category : #accessing }
EpFileLogNode >> globalName [

	^ globalName ifNil: [
		globalName := OmFileStore globalNameFrom: directoryEntry ]
]

{ #category : #initialization }
EpFileLogNode >> initializeWithEntry: aDirectoryEntry [

	directoryEntry := aDirectoryEntry.

	self initialize.
]

{ #category : #accessing }
EpFileLogNode >> log [

	^ log ifNil: [ 
		log := EpLog newWithStore: self ombuStore ]
]

{ #category : #accessing }
EpFileLogNode >> modificationTime [

	^ directoryEntry modificationTime
]

{ #category : #private }
EpFileLogNode >> ombuStore [

	^ OmStoreFactory current fromFile: self fileReference
]
