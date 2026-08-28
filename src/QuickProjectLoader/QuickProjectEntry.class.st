"
I represent a single loadable project entry in the Quick Project Loader.

I hold all necessary metadata to load a project via Metacello from a GitHub repository:
- name: a human-readable display name
- repositoryUrl: the full Metacello repository URL (e.g., 'github://owner/repo:branch/src')
- baselineName: the baseline name without the 'BaselineOf' prefix
- groups: an optional array of group names to load (empty means default group)
- loadCount: how many times this project has been loaded (used for sorting)

Equality is based on repositoryUrl and baselineName, so the same project cannot be added twice.

## Examples

```
QuickProjectEntry 
    name: 'Roassal'
    repositoryUrl: 'github://ObjectProfile/Roassal/src'
    baseline: 'Roassal'

QuickProjectEntry 
    name: 'Roassal'
    repositoryUrl: 'github://ObjectProfile/Roassal/src'
    baseline: 'Roassal'
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
		'loadCount'
	],
	#category : 'QuickProjectLoader-Model',
	#package : 'QuickProjectLoader',
	#tag : 'Model'
}

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

	^ baselineName
]

{ #category : 'accessing' }
QuickProjectEntry >> baselineName: aString [

	baselineName := aString
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
	groups := #()
]

{ #category : 'actions' }
QuickProjectEntry >> load [
	"Execute the Metacello load for this project entry.
	Uses groups if specified, otherwise loads the default group."

	| metacello |
	metacello := Metacello new
		repository: self repositoryUrl;
		baseline: self baselineName;
		yourself.
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
	"Answer a String with the Metacello load expression for this entry.
	Useful for display and debugging purposes."

	^ String streamContents: [ :stream |
		stream
			<< 'Metacello new'; cr;
			<< '  repository: '; << self repositoryUrl printString; << ';'; cr;
			<< '  baseline: '; << self baselineName printString; << ';'; cr.
		self groups 
			ifNotEmpty: [ :g |
				stream << '  load: '; << g printString ]
			ifEmpty: [ 
				stream << '  load' ].
		stream << '.' ]
]

{ #category : 'accessing' }
QuickProjectEntry >> name [

	^ name
]

{ #category : 'accessing' }
QuickProjectEntry >> name: aString [

	name := aString
]

{ #category : 'printing' }
QuickProjectEntry >> printOn: aStream [

	aStream
		<< self name;
		<< ' (';
		<< self baselineName;
		<< ' - loaded ';
		<< self loadCount printString;
		<< ' times)'
]

{ #category : 'accessing' }
QuickProjectEntry >> repositoryUrl [

	^ repositoryUrl
]

{ #category : 'accessing' }
QuickProjectEntry >> repositoryUrl: aString [

	repositoryUrl := aString
]
