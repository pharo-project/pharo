"
I represent a single loadable project entry in the Quick Project Loader.

I hold metadata to load a project via Metacello from a repository:
- name: a human-readable display name
- repositoryUrl: the full Metacello repository URL (e.g., 'github://owner/repo:branch/src')
- baselineName: optional baseline name (without 'BaselineOf' prefix)
- groups: an optional array of group names to load (empty means default group)
- loadCount: how many times this project has been loaded (used for sorting)
- useIncomingOnConflict: boolean indicating whether to send #onConflictUseIncoming to Metacello (default: true)

Equality is based on repositoryUrl and baselineName.

## Examples

```
QuickProjectEntry 
    name: 'Roassal'
    repositoryUrl: 'github://ObjectProfile/Roassal/src'
    baseline: 'Roassal'

QuickProjectEntry 
    name: 'Roassal'
    repositoryUrl: 'github://ObjectProfile/Roassal/src'
    baseline: ''
    groups: #('Full')
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
		'useIncomingOnConflict'
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

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ self repositoryUrl = anObject repositoryUrl
		and: [ self baselineName = anObject baselineName ]
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

	^ self repositoryUrl hash bitXor: self baselineName hash
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
	useIncomingOnConflict := true
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

	^ MetacelloProjectRegistry
		registrationNamed: self name
		ifPresent: [ :reg | reg loadedInImage ]
		ifAbsent: [ false ]
]

{ #category : 'actions' }
QuickProjectEntry >> load [
	"Execute the Metacello load for this project entry.
	Uses baselineName if present, sends #onConflictUseIncoming if requested, and loads specified groups."

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
QuickProjectEntry >> metacelloLoadScript [
	"Answer a String with the Metacello load expression for this entry."

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
