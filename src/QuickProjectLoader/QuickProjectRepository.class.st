"
I manage the persistent collection of QuickProjectEntry objects.

I am a singleton that stores project entries in a STON file at:
  FileLocator preferences / 'pharo' / 'quickProjectLoader.ston'

I answer entries sorted by loadCount (most-used first), persist on every mutation, and gracefully handle a missing file on first load.

## Usage

```
QuickProjectRepository default 
    addEntryNamed: 'Roassal' 
    repository: 'github://ObjectProfile/Roassal/src' 
    baseline: 'Roassal'.

QuickProjectRepository default entries. ""sorted by usage""

QuickProjectRepository default recordLoadOf: anEntry.
```
"
Class {
	#name : 'QuickProjectRepository',
	#superclass : 'Object',
	#instVars : [
		'entries',
		'persistencePath'
	],
	#classVars : [
		'Default'
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
	"Set the default instance. Primarily useful for testing."

	Default := aRepository
]

{ #category : 'class initialization' }
QuickProjectRepository class >> defaultPersistencePath [

	^ FileLocator preferences / 'pharo' / 'quickProjectLoader.ston'
]

{ #category : 'class initialization' }
QuickProjectRepository class >> reset [

	Default := nil
]

{ #category : 'adding' }
QuickProjectRepository >> addEntry: aQuickProjectEntry [
	"Add an entry to the repository. If a duplicate (same URL + baseline) exists, do nothing.
	Saves to disk after adding."

	(self rawEntries anySatisfy: [ :each | each = aQuickProjectEntry ])
		ifTrue: [ ^ aQuickProjectEntry ].
	self rawEntries add: aQuickProjectEntry.
	self save.
	^ aQuickProjectEntry
]

{ #category : 'adding' }
QuickProjectRepository >> addEntryNamed: aName repository: aUrl baseline: aBaselineName [
	"Convenience method to create and add a new entry."

	^ self addEntry: (QuickProjectEntry
		name: aName
		repositoryUrl: aUrl
		baseline: aBaselineName)
]

{ #category : 'adding' }
QuickProjectRepository >> addEntryNamed: aName repository: aUrl baseline: aBaselineName groups: anArray [
	"Convenience method to create and add a new entry with groups."

	^ self addEntry: (QuickProjectEntry
		name: aName
		repositoryUrl: aUrl
		baseline: aBaselineName
		groups: anArray)
]

{ #category : 'accessing' }
QuickProjectRepository >> entries [
	"Answer entries sorted by loadCount descending (most-used first)."

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
	"Load entries from the STON file on disk. 
	If the file does not exist, start with an empty collection."

	| file |
	file := self persistencePath asFileReference.
	file exists ifFalse: [ 
		entries := OrderedCollection new.
		^ self ].
	[ entries := (STON fromString: file contents) asOrderedCollection ]
		on: Error
		do: [ :ex | 
			entries := OrderedCollection new.
			ex return ]
]

{ #category : 'reordering' }
QuickProjectRepository >> moveEntry: aQuickProjectEntry toIndex: anInteger [
	"Move an entry to a specific position in the raw (unsorted) list.
	This allows manual reordering that overrides the loadCount sort."

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
	"Set the persistence path. Primarily useful for testing with temporary files."

	persistencePath := aFileReference
]

{ #category : 'private' }
QuickProjectRepository >> rawEntries [
	"Answer the unsorted entries collection."

	^ entries
]

{ #category : 'actions' }
QuickProjectRepository >> recordLoadOf: aQuickProjectEntry [
	"Increment the load count of the given entry and save."

	aQuickProjectEntry incrementLoadCount.
	self save
]

{ #category : 'removing' }
QuickProjectRepository >> removeEntry: aQuickProjectEntry [
	"Remove an entry from the repository and save."

	self rawEntries remove: aQuickProjectEntry ifAbsent: [].
	self save
]

{ #category : 'persistence' }
QuickProjectRepository >> save [
	"Persist all entries to disk as STON."

	| file |
	file := self persistencePath asFileReference.
	file parent ensureCreateDirectory.
	file writeStreamDo: [ :stream |
		stream nextPutAll: (STON toStringPretty: self rawEntries asArray) ]
]
