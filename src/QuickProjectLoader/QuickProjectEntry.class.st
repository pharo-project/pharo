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
QuickProjectEntry >> displayRepository [
	"Answer the short form shown next to the name, e.g. 'owner/repo'."

	| url path |
	url := self metacelloRepositoryUrl.
	(url includesSubstring: '://') ifFalse: [ ^ url ].
	path := (url splitOn: '://') second substrings: '/'.
	path size < 2 ifTrue: [ ^ url ].
	^ path first , '/' , (path second copyUpTo: $:)
]

{ #category : 'accessing' }
QuickProjectEntry >> effectiveBaselineName [
	"Metacello refuses to run without a baseline, so fall back on the project name."

	| baseline |
	baseline := self baselineName ifEmpty: [ self name ].
	^ (baseline beginsWith: 'BaselineOf')
		ifTrue: [ baseline allButFirst: 'BaselineOf' size ]
		ifFalse: [ baseline ]
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

	"#= matches on any one of name, baseline or URL, so no single field can be hashed."

	^ self class hash
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

	(self class environment hasClassNamed: ('BaselineOf' , self effectiveBaselineName) asSymbol)
		ifTrue: [ ^ true ].

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
		repository: self metacelloRepositoryUrl;
		baseline: self effectiveBaselineName;
		yourself.
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
			<< '  repository: '; << self metacelloRepositoryUrl printString; << ';'; cr;
			<< '  baseline: '; << self effectiveBaselineName printString; << ';'; cr.
		self useIncomingOnConflict ifTrue: [ 
			stream << '  onConflictUseIncoming;'; cr ].
		self groups 
			ifNotEmpty: [ :g | stream << '  load: '; << g printString ]
			ifEmpty: [ stream << '  load' ].
		stream << '.' ]
]

{ #category : 'accessing' }
QuickProjectEntry >> metacelloRepositoryUrl [
	"Answer the URL in a form Metacello accepts: clone URLs pasted from GitHub become github:// ones."

	| url |
	url := self repositoryUrl trimBoth.
	#( 'git@github.com:' 'https://github.com/' 'http://github.com/' ) do: [ :prefix |
		(url beginsWith: prefix) ifTrue: [
			url := 'github://' , (url allButFirst: prefix size).
			^ (url endsWith: '.git')
				ifTrue: [ url allButLast: '.git' size ]
				ifFalse: [ url ] ] ].
	^ url
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
	loadedInSessions := (self loadedInSessions copyWith: aSessionMarker) last: (self loadedInSessions size + 1 min: 30)
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
