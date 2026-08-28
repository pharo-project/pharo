"
I manage the persistent collection of QuickProjectEntry objects.

I am a singleton that stores project entries in a STON file at:
  FileLocator preferences / 'pharo' / 'quickProjectLoader.ston'

I answer entries sorted by loadCount (most-used first), persist on every mutation, and can infer registered projects directly from the Pharo image.

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

	(self rawEntries anySatisfy: [ :each | each = aQuickProjectEntry ])
		ifTrue: [ ^ aQuickProjectEntry ].
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

	self rawEntries ifEmpty: [ self load ].
	self rawEntries ifEmpty: [ self inferProjects ].
	^ self rawEntries sorted: [ :a :b | a loadCount > b loadCount ]
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
			(self rawEntries anySatisfy: [ :e | e baselineName = baseName ]) ifFalse: [ 
				self addEntry: (QuickProjectEntry
					name: baseName
					repositoryUrl: 'github://pharo-project/' , baseName
					baseline: baseName) ] ]
	] on: Error do: [ :ex | "Ignore inference errors" ].

	self save
]

{ #category : 'initialization' }
QuickProjectRepository >> initialize [

	super initialize.
	entries := OrderedCollection new.
	persistencePath := self class defaultPersistencePath
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
			ex return ]
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
	self save
]

{ #category : 'removing' }
QuickProjectRepository >> removeEntry: aQuickProjectEntry [

	self rawEntries remove: aQuickProjectEntry ifAbsent: [].
	self save
]

{ #category : 'persistence' }
QuickProjectRepository >> save [

	| file |
	file := self persistencePath asFileReference.
	file parent ensureCreateDirectory.
	file writeStreamDo: [ :stream |
		stream nextPutAll: (STON toStringPretty: self rawEntries asArray) ]
]
