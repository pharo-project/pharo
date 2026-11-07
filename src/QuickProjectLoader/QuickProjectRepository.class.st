"
I manage the persistent collection of QuickProjectEntry objects.

I am a singleton that stores project entries in a STON file at:
  FileLocator preferences / 'pharo' / 'quickProjectLoader.ston'

The file is shared by every image of the user, and it is the source of truth: the entries I hold are only a cache of it. Before each mutation I re-read it if another image changed it, and I replace it in one step so a shorter list cannot leave a corrupt file behind.

Entries come from real loads only. I subscribe to Metacello's end-of-execution announcement, so anything loaded with a Metacello script is remembered, from any tool, at no cost unless a load happens.

## Usage

```
QuickProjectRepository default visibleEntries. ""manual ones plus recently loaded ones""
```
"
Class {
	#name : 'QuickProjectRepository',
	#superclass : 'Object',
	#instVars : [
		'entries',
		'persistencePath',
		'currentSessionMarker',
		'markerSession',
		'fileStamp'
	],
	#classVars : [
		'Default',
		'RecentImagesWindowSize'
	],
	#category : 'QuickProjectLoader-Model',
	#package : 'QuickProjectLoader',
	#tag : 'Model'
}

{ #category : 'accessing' }
QuickProjectRepository class >> default [

	^ Default ifNil: [ Default := self new ]
]

{ #category : 'accessing' }
QuickProjectRepository class >> default: aRepository [

	Default := aRepository
]

{ #category : 'class initialization' }
QuickProjectRepository class >> defaultPersistencePath [

	^ FileLocator preferences / 'pharo' / 'quickProjectLoader.ston'
]

{ #category : 'class initialization' }
QuickProjectRepository class >> initialize [

	self subscribeToMetacello
]

{ #category : 'announcements' }
QuickProjectRepository class >> metacelloExecutionEnded: anAnnouncement [
	"Must never get in the way of the load that is being reported."

	[ self default recordExecutionOf: anAnnouncement executor ]
		on: Error
		do: [ :ex | ex traceCr ]
]

{ #category : 'accessing' }
QuickProjectRepository class >> recentImagesWindowSize [

	^ RecentImagesWindowSize ifNil: [ RecentImagesWindowSize := 3 ]
]

{ #category : 'accessing' }
QuickProjectRepository class >> recentImagesWindowSize: anInteger [

	RecentImagesWindowSize := anInteger
]

{ #category : 'class initialization' }
QuickProjectRepository class >> reset [

	Default := nil.
	RecentImagesWindowSize := nil
]

{ #category : 'announcements' }
QuickProjectRepository class >> subscribeToMetacello [
	<script>

	self codeSupportAnnouncer unsubscribe: self.
	self codeSupportAnnouncer
		when: MetacelloExecutionEndAnnouncement
		do: [ :announcement | self metacelloExecutionEnded: announcement ]
		for: self
]

{ #category : 'adding' }
QuickProjectRepository >> addEntry: aQuickProjectEntry [
	"Add an entry to the repository. If a duplicate exists, update missing info instead of duplicating."

	| existing |
	self syncWithDisk.
	existing := self rawEntries detect: [ :each | each = aQuickProjectEntry ] ifNone: [ nil ].
	existing ifNil: [
		self rawEntries add: aQuickProjectEntry.
		self save.
		^ aQuickProjectEntry ].

	existing repositoryUrl ifEmpty: [ existing repositoryUrl: aQuickProjectEntry repositoryUrl ].
	existing baselineName ifEmpty: [ existing baselineName: aQuickProjectEntry baselineName ].
	aQuickProjectEntry isManual ifTrue: [ existing isManual: true ].
	self save.
	^ existing
]

{ #category : 'adding' }
QuickProjectRepository >> addEntryNamed: aName repository: aUrl baseline: aBaselineName [

	^ self addEntry: (QuickProjectEntry
		name: aName
		repositoryUrl: aUrl
		baseline: aBaselineName)
]

{ #category : 'adding' }
QuickProjectRepository >> addEntryNamed: aName repository: aUrl baseline: aBaselineName groups: anArray [

	^ self addEntry: (QuickProjectEntry
		name: aName
		repositoryUrl: aUrl
		baseline: aBaselineName
		groups: anArray)
]

{ #category : 'accessing' }
QuickProjectRepository >> currentSessionMarker [

	"Answered lazily so that a new image session costs nothing at startup."

	markerSession == Smalltalk session ifFalse: [
		markerSession := Smalltalk session.
		currentSessionMarker := DateAndTime now ].
	^ currentSessionMarker
]

{ #category : 'accessing' }
QuickProjectRepository >> currentSessionMarker: anObject [

	markerSession := Smalltalk session.
	currentSessionMarker := anObject
]

{ #category : 'accessing' }
QuickProjectRepository >> entries [
	"Answer all deduplicated entries in the store, most-loaded first."

	self sanitizeEntries.
	^ self rawEntries sorted: [ :a :b | a loadCount > b loadCount ]
]

{ #category : 'testing' }
QuickProjectRepository >> hasEntries [

	^ self rawEntries isNotEmpty
]

{ #category : 'initialization' }
QuickProjectRepository >> initialize [

	super initialize.
	entries := OrderedCollection new.
	persistencePath := self class defaultPersistencePath
]

{ #category : 'persistence' }
QuickProjectRepository >> load [
	"Replace the cache with the file. An unreadable file keeps the cache as it is.
	 Entries that are neither manual nor ever loaded carry no history: older versions wrote them by guessing from the image, so they are dropped here."

	| file |
	file := self persistencePath asFileReference.
	file exists ifFalse: [ 
		entries := OrderedCollection new.
		fileStamp := nil.
		^ self ].
	[ entries := (STON fromString: file contents) asOrderedCollection
		reject: [ :each | each isManual not and: [ each loadedInSessions isEmpty ] ] ]
		on: Error
		do: [ :ex | ex traceCr ].
	fileStamp := self stampOf: file.
	self sanitizeEntries
]

{ #category : 'reordering' }
QuickProjectRepository >> moveEntry: aQuickProjectEntry toIndex: anInteger [

	| index |
	index := self rawEntries indexOf: aQuickProjectEntry.
	index = 0 ifTrue: [ ^ self ].
	self rawEntries remove: aQuickProjectEntry.
	self rawEntries add: aQuickProjectEntry beforeIndex: (anInteger min: self rawEntries size + 1).
	self save
]

{ #category : 'accessing' }
QuickProjectRepository >> persistencePath [

	^ persistencePath
]

{ #category : 'accessing' }
QuickProjectRepository >> persistencePath: aFileReference [

	persistencePath := aFileReference
]

{ #category : 'private' }
QuickProjectRepository >> rawEntries [

	^ entries
]

{ #category : 'private' }
QuickProjectRepository >> recentSessionMarkers [
	"Answer the markers of the last N distinct sessions seen across all entries, newest-first."

	| seenMarkers |
	seenMarkers := Set new.
	self rawEntries do: [ :entry | seenMarkers addAll: entry loadedInSessions ].
	^ (seenMarkers asArray sort: [ :a :b | a >= b ])
		first: (seenMarkers size min: self class recentImagesWindowSize)
]

{ #category : 'actions' }
QuickProjectRepository >> recordExecutionOf: aMetacelloExecutor [
	"Remember a finished Metacello load. The announcement also comes after a failed load, and a failed load never brought its baseline in."

	| action entry |
	action := aMetacelloExecutor actionArg.
	(action notNil and: [ action key = #load: ]) ifFalse: [ ^ self ].
	aMetacelloExecutor repositoryArg ifNil: [ ^ self ].
	entry := QuickProjectEntry
		name: (aMetacelloExecutor baselineArg asString withoutPrefix: 'BaselineOf')
		repositoryUrl: aMetacelloExecutor repositoryArg asString
		baseline: (aMetacelloExecutor baselineArg asString withoutPrefix: 'BaselineOf')
		groups: action value first asArray.
	entry isLoadedInImage ifTrue: [ self recordLoadOf: entry ]
]

{ #category : 'actions' }
QuickProjectRepository >> recordLoadOf: aQuickProjectEntry [

	| entry |
	self syncWithDisk.
	entry := self rawEntries
		detect: [ :each | each = aQuickProjectEntry ]
		ifNone: [ self rawEntries add: aQuickProjectEntry ].
	entry incrementLoadCount.
	entry recordLoadInSession: self currentSessionMarker.
	self save.
	^ entry
]

{ #category : 'removing' }
QuickProjectRepository >> removeEntry: aQuickProjectEntry [

	self syncWithDisk.
	self rawEntries remove: aQuickProjectEntry ifAbsent: [].
	self save
]

{ #category : 'private' }
QuickProjectRepository >> sanitizeEntries [
	"Remove duplicate entries that match by name, baselineName, or repositoryUrl."

	| unique |
	unique := OrderedCollection new.
	self rawEntries do: [ :entry |
		(unique anySatisfy: [ :existing | existing = entry ])
			ifFalse: [ unique add: entry ] ].
	entries := unique
]

{ #category : 'persistence' }
QuickProjectRepository >> save [
	"Write a new file and swap it in: a write stream on the existing file would keep its tail whenever the new contents are shorter."

	| file temporary |
	file := self persistencePath asFileReference.
	file parent ensureCreateDirectory.
	temporary := file parent / (file basename , '.tmp').
	temporary ensureDelete.
	temporary writeStreamDo: [ :stream |
		stream nextPutAll: (STON toStringPretty: self rawEntries asArray) ].
	file ensureDelete.
	temporary moveTo: file.
	fileStamp := self stampOf: file
]

{ #category : 'private' }
QuickProjectRepository >> stampOf: aFileReference [

	^ { aFileReference modificationTime. aFileReference size }
]

{ #category : 'persistence' }
QuickProjectRepository >> syncWithDisk [
	"Reload only if the file is not the one this repository last read or wrote, typically because another image changed it."

	| file |
	file := self persistencePath asFileReference.
	(file exists and: [ fileStamp ~= (self stampOf: file) ]) ifTrue: [ self load ]
]

{ #category : 'accessing' }
QuickProjectRepository >> visibleEntries [
	"Answer the entries to suggest: manually added ones plus those loaded in one of the last N image sessions, most-recently-used first."

	| markers rank |
	markers := self recentSessionMarkers.
	rank := [ :entry |
		markers
			detectIndex: [ :marker | entry loadedInSessions includes: marker ]
			ifNone: [ markers size + 1 ] ].
	self sanitizeEntries.
	^ (self rawEntries select: [ :entry | entry isManual or: [ (rank value: entry) <= markers size ] ])
		sorted: [ :a :b |
			| rankA rankB |
			rankA := rank value: a.
			rankB := rank value: b.
			rankA < rankB or: [ rankA = rankB and: [ a name <= b name ] ] ]
]
