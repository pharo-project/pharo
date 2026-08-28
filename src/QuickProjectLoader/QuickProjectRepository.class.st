"
I manage the persistent collection of QuickProjectEntry objects.

I am a singleton that stores project entries in a STON file at:
  FileLocator preferences / 'pharo' / 'quickProjectLoader.ston'

I answer entries sorted by loadCount (most-used first), deduplicated by project name/baseline, persist on every mutation, and can infer registered projects directly from the Pharo image.

## Usage

```
QuickProjectRepository default inferProjects. ""Auto-populate entries from image""
QuickProjectRepository default entries. ""sorted by usage""
```
"
Class {
	#name : 'QuickProjectRepository',
	#superclass : 'Object',
	#instVars : [
		'entries',
		'persistencePath',
		'currentSessionMarker'
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

{ #category : 'accessing' }
QuickProjectRepository class >> recentImagesWindowSize [

	^ RecentImagesWindowSize ifNil: [ RecentImagesWindowSize := 3 ]
]

{ #category : 'accessing' }
QuickProjectRepository class >> recentImagesWindowSize: anInteger [

	RecentImagesWindowSize := anInteger
]

{ #category : 'class initialization' }
QuickProjectRepository class >> defaultPersistencePath [

	^ FileLocator preferences / 'pharo' / 'quickProjectLoader.ston'
]

{ #category : 'class initialization' }
QuickProjectRepository class >> reset [

	Default := nil.
	RecentImagesWindowSize := nil
]

{ #category : 'adding' }
QuickProjectRepository >> addEntry: aQuickProjectEntry [
	"Add an entry to the repository. If a duplicate exists, update missing info instead of duplicating."

	| existing |
	existing := self rawEntries detect: [ :each | each = aQuickProjectEntry ] ifNone: [ nil ].
	existing ifNotNil: [ 
		existing repositoryUrl ifEmpty: [ existing repositoryUrl: aQuickProjectEntry repositoryUrl ].
		existing baselineName ifEmpty: [ existing baselineName: aQuickProjectEntry baselineName ].
		self save.
		^ existing ].

	self rawEntries add: aQuickProjectEntry.
	self save.
	^ aQuickProjectEntry
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
QuickProjectRepository >> entries [
	"Answer all deduplicated entries in the store, most-loaded first."

	self sanitizeEntries.
	self rawEntries ifEmpty: [ self load ].
	self rawEntries ifEmpty: [ self inferProjects ].
	self sanitizeEntries.
	^ self rawEntries sorted: [ :a :b | a loadCount > b loadCount ]
]

{ #category : 'accessing' }
QuickProjectRepository >> visibleEntries [
	"Answer the entries to suggest: manually added ones plus those loaded in one of the last N image sessions, most-recently-used first."

	self entries.
	^ (self rawEntries select: [ :entry | self isEntryVisible: entry ])
		sorted: [ :a :b |
			| rankA rankB |
			rankA := self rankOf: a.
			rankB := self rankOf: b.
			rankA < rankB or: [ rankA = rankB and: [ a name <= b name ] ] ]
]

{ #category : 'private' }
QuickProjectRepository >> rankOf: anEntry [
	"1 = most recent load. Entries are ranked by the most recent session in which they were loaded; entries never loaded in a recent session sink to the bottom."

	^ self recentSessionMarkers
		detectIndex: [ :marker | anEntry loadedInSessions includes: marker ]
		ifNone: [ self recentSessionMarkers size + 1 ]
]

{ #category : 'private' }
QuickProjectRepository >> isEntryVisible: anEntry [

	^ anEntry isManual
		or: [ anEntry loadedInSessions anySatisfy: [ :m | self recentSessionMarkers includes: m ] ]
]

{ #category : 'private' }
QuickProjectRepository >> recentSessionMarkers [
	"Answer the markers of the last N distinct sessions seen across all entries, newest-first."

	| seenMarkers |
	seenMarkers := OrderedCollection new.
	self rawEntries do: [ :entry |
		entry loadedInSessions do: [ :marker |
			(seenMarkers includes: marker) ifFalse: [ seenMarkers add: marker ] ] ].
	seenMarkers sort: [ :a :b | a >= b ].
	^ seenMarkers first: (seenMarkers size min: self class recentImagesWindowSize)
]

{ #category : 'testing' }
QuickProjectRepository >> hasEntries [

	^ self rawEntries isNotEmpty
]

{ #category : 'inference' }
QuickProjectRepository >> inferProjects [
	"Infer projects automatically from MetacelloProjectRegistry and BaselineOf subclasses in the Pharo image."

	"1. From MetacelloProjectRegistry registrations"
	[ 
		(MetacelloProjectRegistry registry registrations) do: [ :reg |
			| spec repoUrl baseName projName |
			spec := reg projectSpec.
			spec ifNotNil: [
				projName := spec name.
				baseName := (projName beginsWith: 'BaselineOf')
					ifTrue: [ projName copyFrom: 11 to: projName size ]
					ifFalse: [ projName ].
				repoUrl := (spec respondsTo: #repositorySpecs)
					ifTrue: [ 
						spec repositorySpecs 
							ifNotEmpty: [ :repos | repos first description ]
							ifEmpty: [ '' ] ]
					ifFalse: [ '' ].
				repoUrl ifNotEmpty: [ 
					self addEntry: (QuickProjectEntry
						name: baseName
						repositoryUrl: repoUrl
						baseline: baseName) ] ] ]
	] on: Error do: [ :ex | "Ignore inference errors" ].

	"2. From loaded BaselineOf subclasses"
	[
		BaselineOf allSubclasses do: [ :cls |
			| baseName |
			baseName := (cls name beginsWith: 'BaselineOf')
				ifTrue: [ cls name copyFrom: 11 to: cls name size ]
				ifFalse: [ cls name ].
			self addEntry: (QuickProjectEntry
				name: baseName
				repositoryUrl: 'github://pharo-project/' , baseName
				baseline: baseName) ]
	] on: Error do: [ :ex | "Ignore inference errors" ].

	self sanitizeEntries.
	self save
]

{ #category : 'initialization' }
QuickProjectRepository >> initialize [

	super initialize.
	entries := OrderedCollection new.
	persistencePath := self class defaultPersistencePath.
	currentSessionMarker := DateAndTime now
]

{ #category : 'accessing' }
QuickProjectRepository >> currentSessionMarker [

	^ currentSessionMarker
]

{ #category : 'accessing' }
QuickProjectRepository >> currentSessionMarker: anObject [

	currentSessionMarker := anObject
]

{ #category : 'persistence' }
QuickProjectRepository >> load [

	| file |
	file := self persistencePath asFileReference.
	file exists ifFalse: [ 
		entries := OrderedCollection new.
		^ self ].
	[ entries := (STON fromString: file contents) asOrderedCollection ]
		on: Error
		do: [ :ex | 
			entries := OrderedCollection new.
			ex return ].
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

{ #category : 'actions' }
QuickProjectRepository >> recordLoadOf: aQuickProjectEntry [

	aQuickProjectEntry incrementLoadCount.
	aQuickProjectEntry recordLoadInSession: self currentSessionMarker.
	self save
]

{ #category : 'removing' }
QuickProjectRepository >> removeEntry: aQuickProjectEntry [

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

	| file |
	file := self persistencePath asFileReference.
	file parent ensureCreateDirectory.
	file writeStreamDo: [ :stream |
		stream nextPutAll: (STON toStringPretty: self rawEntries asArray) ]
]
