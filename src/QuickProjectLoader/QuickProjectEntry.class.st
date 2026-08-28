"
I represent a single loadable project entry in the Quick Project Loader.

I hold metadata to load a project via Metacello from a repository:
- name: a human-readable display name
- repositoryUrl: the full Metacello repository URL (e.g., 'github://owner/repo:branch/src')
- baselineName: optional baseline name (without 'BaselineOf' prefix)
- groups: an optional array of group names to load (empty means default group)
- loadCount: how many times this project has been loaded (used for sorting)
- useIncomingOnConflict: boolean indicating whether to send #onConflictUseIncoming to Metacello (default: true)

Equality is based on matching name, baselineName, or repositoryUrl (case-insensitive) to prevent duplicate projects.

## Examples

```
QuickProjectEntry 
    name: 'Roassal'
    repositoryUrl: 'github://ObjectProfile/Roassal/src'
    baseline: 'Roassal'
```
"
Class {
	#name : 'QuickProjectEntry',
	#superclass : 'Object',
	#instVars : [
		'name',
		'repositoryUrl',
		'baselineName',
		'groups',
		'loadCount',
		'useIncomingOnConflict',
		'loadedInSessions',
		'isManual'
	],
	#category : 'QuickProjectLoader-Model',
	#package : 'QuickProjectLoader',
	#tag : 'Model'
}

{ #category : 'instance creation' }
QuickProjectEntry class >> name: aName repositoryUrl: aUrl [

	^ self new
		name: aName;
		repositoryUrl: aUrl;
		baselineName: '';
		yourself
]

{ #category : 'instance creation' }
QuickProjectEntry class >> name: aName repositoryUrl: aUrl baseline: aBaselineName [

	^ self new
		name: aName;
		repositoryUrl: aUrl;
		baselineName: aBaselineName;
		yourself
]

{ #category : 'instance creation' }
QuickProjectEntry class >> name: aName repositoryUrl: aUrl baseline: aBaselineName groups: anArray [

	^ self new
		name: aName;
		repositoryUrl: aUrl;
		baselineName: aBaselineName;
		groups: anArray;
		yourself
]

{ #category : 'comparing' }
QuickProjectEntry >> = anObject [
	"Two entries are equal if they share the same name, baselineName, or repositoryUrl."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].

	(self name isNotEmpty and: [ anObject name isNotEmpty and: [ self name asLowercase = anObject name asLowercase ] ])
		ifTrue: [ ^ true ].

	(self baselineName isNotEmpty and: [ anObject baselineName isNotEmpty and: [ self baselineName asLowercase = anObject baselineName asLowercase ] ])
		ifTrue: [ ^ true ].

	^ self repositoryUrl isNotEmpty 
		and: [ anObject repositoryUrl isNotEmpty 
		and: [ self repositoryUrl asLowercase = anObject repositoryUrl asLowercase ] ]
]

{ #category : 'accessing' }
QuickProjectEntry >> baselineName [

	^ baselineName ifNil: [ baselineName := '' ]
]

{ #category : 'accessing' }
QuickProjectEntry >> baselineName: aString [

	baselineName := aString ifNil: [ '' ]
]

{ #category : 'accessing' }
QuickProjectEntry >> groups [

	^ groups ifNil: [ groups := #() ]
]

{ #category : 'accessing' }
QuickProjectEntry >> groups: anArray [

	groups := anArray
]

{ #category : 'comparing' }
QuickProjectEntry >> hash [

	self name isNotEmpty ifTrue: [ ^ self name asLowercase hash ].
	self baselineName isNotEmpty ifTrue: [ ^ self baselineName asLowercase hash ].
	^ self repositoryUrl asLowercase hash
]

{ #category : 'actions' }
QuickProjectEntry >> incrementLoadCount [

	loadCount := self loadCount + 1
]

{ #category : 'initialization' }
QuickProjectEntry >> initialize [

	super initialize.
	loadCount := 0.
	groups := #().
	baselineName := ''.
	useIncomingOnConflict := true.
	loadedInSessions := #().
	isManual := false
]

{ #category : 'testing' }
QuickProjectEntry >> isLoadedInImage [
	"Answer true if this project (baseline class or registered project) is currently loaded in the image."

	self baselineName isNotEmpty ifTrue: [ 
		| clsName |
		clsName := (self baselineName beginsWith: 'BaselineOf')
			ifTrue: [ self baselineName ]
			ifFalse: [ 'BaselineOf' , self baselineName ].
		(self class environment hasClassNamed: clsName asSymbol) ifTrue: [ ^ true ] ].

	self name isNotEmpty ifTrue: [ 
		| clsName |
		clsName := (self name beginsWith: 'BaselineOf')
			ifTrue: [ self name ]
			ifFalse: [ 'BaselineOf' , self name ].
		(self class environment hasClassNamed: clsName asSymbol) ifTrue: [ ^ true ] ].

	^ MetacelloProjectRegistry
		registrationNamed: self name
		ifPresent: [ :reg | reg loadedInImage ]
		ifAbsent: [ false ]
]

{ #category : 'accessing' }
QuickProjectEntry >> isManual [

	^ isManual ifNil: [ isManual := false ]
]

{ #category : 'accessing' }
QuickProjectEntry >> isManual: aBoolean [

	isManual := aBoolean
]

{ #category : 'actions' }
QuickProjectEntry >> load [

	| metacello |
	metacello := Metacello new
		repository: self repositoryUrl;
		yourself.
	self baselineName isNotEmpty ifTrue: [ 
		metacello baseline: self baselineName ].
	self useIncomingOnConflict ifTrue: [ 
		metacello onConflictUseIncoming ].
	self groups
		ifEmpty: [ metacello load ]
		ifNotEmpty: [ :g | metacello load: g ]
]

{ #category : 'accessing' }
QuickProjectEntry >> loadCount [

	^ loadCount ifNil: [ loadCount := 0 ]
]

{ #category : 'accessing' }
QuickProjectEntry >> loadCount: anInteger [

	loadCount := anInteger
]

{ #category : 'accessing' }
QuickProjectEntry >> loadedInSessions [

	^ loadedInSessions ifNil: [ loadedInSessions := #() ]
]

{ #category : 'accessing' }
QuickProjectEntry >> loadedInSessions: aCollection [

	loadedInSessions := aCollection
]

{ #category : 'accessing' }
QuickProjectEntry >> metacelloLoadScript [

	^ String streamContents: [ :stream |
		stream
			<< 'Metacello new'; cr;
			<< '  repository: '; << self repositoryUrl printString; << ';'; cr.
		self baselineName isNotEmpty ifTrue: [ 
			stream << '  baseline: '; << self baselineName printString; << ';'; cr ].
		self useIncomingOnConflict ifTrue: [ 
			stream << '  onConflictUseIncoming;'; cr ].
		self groups 
			ifNotEmpty: [ :g | stream << '  load: '; << g printString ]
			ifEmpty: [ stream << '  load' ].
		stream << '.' ]
]

{ #category : 'accessing' }
QuickProjectEntry >> name [

	^ name ifNil: [ name := '' ]
]

{ #category : 'accessing' }
QuickProjectEntry >> name: aString [

	name := aString
]

{ #category : 'printing' }
QuickProjectEntry >> printOn: aStream [

	aStream << self name.
	self baselineName isNotEmpty ifTrue: [ 
		aStream << ' (' << self baselineName << ')' ].
	aStream << ' - loaded ' << self loadCount printString << ' times'
]

{ #category : 'actions' }
QuickProjectEntry >> recordLoadInSession: aSessionMarker [

	(self loadedInSessions includes: aSessionMarker) ifTrue: [ ^ self ].
	loadedInSessions := (self loadedInSessions copyWith: aSessionMarker)
		last: 30;
		yourself.
	^ self
]

{ #category : 'accessing' }
QuickProjectEntry >> repositoryUrl [

	^ repositoryUrl ifNil: [ repositoryUrl := '' ]
]

{ #category : 'accessing' }
QuickProjectEntry >> repositoryUrl: aString [

	repositoryUrl := aString
]

{ #category : 'accessing' }
QuickProjectEntry >> useIncomingOnConflict [

	^ useIncomingOnConflict ifNil: [ useIncomingOnConflict := true ]
]

{ #category : 'accessing' }
QuickProjectEntry >> useIncomingOnConflict: aBoolean [

	useIncomingOnConflict := aBoolean
]
