"
I'm a monticello tool composed of three panes to browse repositories and the packages they contain. You get an instance of me when you click on a repository in Monticello browser and press open. 

My left pane presents the packages, my right one their versions and the bottom one the commit log of the selected package versions.

I underline the packages you already loaded, and highlight the ones you don't have updated to the last version. I also highlight the versions you did not load yet.

Example: I can browse packages of PharoInbox with:
(MCFileRepositoryInspector 
	repository: (MCHttpRepository
					location: 'http://www.squeaksource.com/PharoInbox'
					user: ''
					password: '')
	workingCopy: nil) show.
	
COTDC - S.Ducasse, G.Polito, L.Laffont
"
Class {
	#name : #MCFileRepositoryInspector,
	#superclass : #MCVersionInspector,
	#instVars : [
		'repository',
		'versions',
		'loaded',
		'loadedPackages',
		'newer',
		'packageList',
		'selectedPackage',
		'selectedVersion',
		'order',
		'versionInfo',
		'packagePattern',
		'versionPattern',
		'packageListUpdateProcess',
		'versionProcess',
		'selectedPackageAncestors'
	],
	#classVars : [
		'Order'
	],
	#classInstVars : [
		'packageSearchList',
		'versionSearchList'
	],
	#category : #MonticelloGUI
}

{ #category : #'as yet unclassified' }
MCFileRepositoryInspector class >> order [
	^ Order ifNil: [ Order := 5 ]
]

{ #category : #'as yet unclassified' }
MCFileRepositoryInspector class >> order: anInteger [
	Order := anInteger
]

{ #category : #'as yet unclassified' }
MCFileRepositoryInspector class >> packageSearchList [
	^ packageSearchList ifNil: [ packageSearchList := OrderedCollection new]. 
]

{ #category : #'as yet unclassified' }
MCFileRepositoryInspector class >> repository: aFileBasedRepository workingCopy: aWorkingCopy [
	^self new
		setRepository: aFileBasedRepository workingCopy: aWorkingCopy;
		yourself
]

{ #category : #'as yet unclassified' }
MCFileRepositoryInspector class >> versionSearchList [
	^ versionSearchList ifNil: [ versionSearchList := OrderedCollection new]. 
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> buttonSpecs [
	^#(('Refresh' refresh 'refresh the version-list')
		(Save saveChanges 'Save the local changes')) , super buttonSpecs
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> defaultLabel [
	^'Repository: ' , repository description
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> hasVersion [
	^ selectedVersion notNil
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> initialExtent [
	^ 640 @ 480
]

{ #category : #initialization }
MCFileRepositoryInspector >> initialize [
	super initialize.
	loaded := Set new.
	loadedPackages := Set new.
	versionPattern := ''.
	packagePattern := ''.
	packageList := #()
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> invalidatePackageAncestryCache [
	"Invalidate the ancestry cache so the ancestor list is correctly styled/formatted in the version list i.e., underlined if loaded, bold if not loaded"
	selectedPackageAncestors := nil
]

{ #category : #actions }
MCFileRepositoryInspector >> load [
	self hasVersion
		ifFalse: [ ^ self ].
	self version isCacheable
		ifTrue: [ version workingCopy repositoryGroup addRepository: repository ].
	super load.
	self invalidatePackageAncestryCache.
	self refresh
]

{ #category : #actions }
MCFileRepositoryInspector >> merge [
	super merge.
	self invalidatePackageAncestryCache.
	self refresh.

]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> order: anInteger [
	self class order: (order := anInteger).
	self changed: #versionList.
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> orderSpecs [
	^{
		'Unchanged' -> nil.
		'Order by package' -> [ :x :y | x first <= y first ].
		'Order by author' -> [ :x :y | x second <= y second ].
		'Order by version-string' -> [ :x :y | x third <= y third ].
		'Order by version-number' -> [ :x :y | x third asNumber >= y third asNumber ].
		'Order by filename' -> [ :x :y | x fourth <= y fourth ].
	}
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> orderString: anIndex [
	^String streamContents: [ :stream |
		order = anIndex
			ifTrue: [ stream nextPutAll: '<yes>' ]
			ifFalse: [ stream nextPutAll: '<no>' ].
		stream nextPutAll: (self orderSpecs at: anIndex) key ]
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> packageIcon: aPackage [
	| p |
	newer ifNil: [ newer := #() ].
	(loaded anySatisfy: [ :each | each beginsWith: aPackage , '-' ])
		ifFalse: [ ^ self iconNamed: #emptyPackageIcon ].

	"If newer collection includes our package it means we have an older mcz version loaded"
	(newer anySatisfy: [ :each | each beginsWith: aPackage , '-' ])
		ifTrue: [ ^ self iconNamed: #newerPackagesAvailableIcon ].
	p := RPackageOrganizer default
		packageNamed: aPackage
		ifAbsent: [ nil ].
	p ifNil: [ ^ self iconNamed: #emptyPackageIcon ].
	^ p isDirty
		ifFalse: [ self iconNamed: #packageIcon ]
		ifTrue: [ self iconNamed: #dirtyPackageIcon ]
]

{ #category : #accessing }
MCFileRepositoryInspector >> packageList [
	^ packageList 
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> packageListMenu: aMenu [
	^aMenu
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> packageListUpdate [
	| result packageNames |	
	
	"Within fork, make no changes to object state until end with assignment wrapped by #defer:"
	
	packageListUpdateProcess ifNotNil: [ packageListUpdateProcess terminate ].	
	packageListUpdateProcess := 
	[	versions ifNotNil: 
		[	result := Set new: versions size.
			versions do: [ :each | result add: each first ].
			
			"sort loaded packages first, then alphabetically"
			packageNames := Set new: loaded size.
			loaded do: [ :each |
				packageNames add: (each copyUpToLast: $-). ].
			
			result := result asArray sort: [ :a :b |
				| loadedA loadedB |
				loadedA := packageNames includes: a.
				loadedB := packageNames includes: b.
				loadedA = loadedB 
					ifTrue: [ a < b ]
					ifFalse: [ loadedA ] ].
	
			packagePattern ifNotEmpty: [
				result := result select: [ :packageName |  packageName asLowercase includesSubstring: packagePattern ]].
			 
			UIManager default defer:
			[	packageList := result.
				self changed: #packageList.
				self changed: #versionList.
			]
		]
	] fork.
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> packageSearchAccept: string [
	| aString |

	aString := string ifNil: [ '' ].
	packagePattern = aString asLowercase ifTrue: [ ^ self ].
	packagePattern :=  aString asLowercase.

   	self packageListUpdate.
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> packageSearchField [
	^ SearchMorph new
		model: self;
		setIndexSelector: #packageSearchAccept:;
		updateSelector: #packageSearchAccept:;
		searchList: self class packageSearchList;
		yourself
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> packageSelection [
	^self packageList ifNotNil: [
		^ self packageList indexOf: selectedPackage
	]
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> packageSelection: aNumber [
	"Track a package change to have the ancestors list loaded."

	"selectedPackage is the selected package name :("

	selectedPackage := aNumber isZero
		ifFalse: [ (self packageList at: aNumber) asString ].
	selectedPackageAncestors := nil.
	self versionSelection: 0.
	self
		changed: #packageSelection;
		changed: #versionList
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> refresh [
	loadedPackages := Set new.
	versions := repository retrieveVersionsWithPackageNames: Set new.
	newer := Set new.
	loaded := Set new.
	MCWorkingCopy allManagers
		do: [ :workingCopy | | versionsForPackage |
			workingCopy ancestors do: [ :ancestor | loaded add: ancestor name ].
			versionsForPackage := versions select: [ :v | v first = workingCopy package name ].
			versionsForPackage
				do: [ :aVersionArray | 
					workingCopy ancestors
						do: [ :ancestor |
							(ancestor versionNumber < aVersionArray third
								or: [ ancestor versionNumber = aVersionArray third and: [ ancestor author ~= aVersionArray second ] ])
								ifTrue: [ newer add: ancestor name ] ] ] ].
					
	self
		packageListUpdate;
		changed: #versionList
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> saveChanges [
	| currentRepository workingCopy |
	
	currentRepository := repository. "This can be changed elsewhere while processing"
	workingCopy := MCWorkingCopy forPackage: (MCPackage new name: selectedPackage).
	(workingCopy newVersionIn: currentRepository) ifNotNil: [:v |
		(MCVersionInspector new version: v) show.
		Cursor wait showWhile: [currentRepository storeVersion: v].
		MCCacheRepository uniqueInstance cacheAllFileNamesDuring: 
			[currentRepository cacheAllFileNamesDuring: 
				[v allAvailableDependenciesDo:
					[:dep |
					(currentRepository includesVersionNamed: dep info name)
						ifFalse: [currentRepository storeVersion: dep]]]]]
]

{ #category : #accessing }
MCFileRepositoryInspector >> selectedPackageAncestors [
	selectedPackageAncestors
		ifNil: [ 
			selectedPackage
				ifNotNil: [ 
					(MCWorkingCopy registry at: (MCPackage named: selectedPackage) ifAbsent: [  ])
						ifNotNil: [ :aWorkingCopy | selectedPackageAncestors := aWorkingCopy ancestry breadthFirstAncestors collect: #name ] ] ].
	^ selectedPackageAncestors
]

{ #category : #accessing }
MCFileRepositoryInspector >> setRepository: aFileBasedRepository workingCopy: aWorkingCopy [
	order := self class order.
	repository := aFileBasedRepository.
	
	aWorkingCopy
		ifNil: [ selectedPackage := self packageList isEmpty ifFalse: [self packageList first asString]]
		ifNotNil: [ selectedPackage := aWorkingCopy ancestry ancestorString copyUpToLast: $-.
			selectedPackage ifEmpty: [
				selectedPackage := aWorkingCopy package name ]]. 
	
	[ self refresh.
	MCWorkingCopy addDependent: self.] fork.

]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> version [
	^ version ifNil:
		[Cursor wait showWhile:
			[version := repository versionFromFileNamed: selectedVersion]]
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> versionHighlight: aString [

	^ Text
		string: aString
		attribute: (self versionHighlightEmphasis: aString)
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> versionHighlightEmphasis: aString [
	"Appearence. This code attempts to show the following:
		- a currently loaded version of the package: underlined.
		- a version of a not loaded package: bold.
		- a newer version of a loaded package: bold.
		- a version not in the ancestry of the currently loaded and
		selected package: bold.
	All those tests are made on the version name and number, not the package
	UUID, so they are not reliable. The least reliable test is the 'newer' check.
	"

	| versionName packageName |
	versionName := (aString copyUpToLast: $.) copyUpTo: $(.
	packageName := versionName copyUpToLast: $-.
	(packageName includes: $.) ifTrue: [ packageName := packageName copyUpTo: $. ].
	(loaded includes: versionName) ifTrue: [ ^ TextEmphasis underlined ].
	selectedPackage = packageName ifTrue: [ 
		self selectedPackageAncestors
			ifNotNil: [
				(selectedPackageAncestors includes: versionName)
					ifTrue: [ ^ TextEmphasis normal ]
					ifFalse: [ ^ TextEmphasis bold ] ] ].
	(newer includes: versionName) ifTrue: [ ^ TextEmphasis bold ].
	(loadedPackages includes: packageName) ifFalse: [ ^ TextEmphasis bold ].
	^ TextEmphasis normal
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> versionInfo [
	^ versionInfo ifNil: [versionInfo := repository versionInfoFromFileNamed: selectedVersion]
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> versionList [
	| result sortBlock |
	result := selectedPackage 
		ifNil: [ versions ifNil: [ ^ #() ]]
		ifNotNil: [ (versions ifNil: [ ^ #() ]) select: [ :each | selectedPackage = each first ] ].
	sortBlock := (self orderSpecs at: order) value.
	sortBlock ifNotNil: [
		result := result asSortedCollection: [:a :b | [sortBlock value: a value: b] on: Error do: [true]]].
	
	versionPattern ifNotEmpty: [
		result := result select: [ :package| package fourth asLowercase includesSubstring: versionPattern ]].
					
	^ result collect: [ :each | self versionHighlight: each fourth ]
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> versionListMenu: aMenu [
	1 to: self orderSpecs size do: [ :index |
		aMenu addUpdating: #orderString: target: self selector: #order: argumentList: { index } ].
	^aMenu
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> versionSearchAccept: string [
	| aString |

	aString := string ifNil: [ '' ].
	versionPattern = aString asLowercase ifTrue: [ ^ self ].
	versionPattern := aString asLowercase.

	versionProcess ifNotNil: [ versionProcess terminate ].
	versionProcess := [ self changed: #versionList ] fork.
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> versionSearchField [
	^ SearchMorph new
		model: self;
		setIndexSelector: #versionSearchAccept:;
		updateSelector: #versionSearchAccept:;
		searchList: self class versionSearchList;
		yourself
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> versionSelection [
	^self versionList indexOf: selectedVersion
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> versionSelection: aNumber [
	aNumber isZero 
		ifTrue: [ selectedVersion := version := versionInfo := nil ]
		ifFalse: [ 
			selectedVersion := (self versionList at: aNumber) asString.
			version := versionInfo := nil].
	self changed: #versionSelection; changed: #summary; changed: #hasVersion
]

{ #category : #'morphic ui' }
MCFileRepositoryInspector >> widgetSpecs [
	| searchBarOffset |
	
	searchBarOffset := 30 + StandardFonts defaultFont height + 10..
	
	^{ {{#buttonRow}.            {0. 0. 1. 0}.     {0. 0. 0. 30}}.
		
		{{#packageSearchField}.   {0. 0. 0.5. 0}.   {0. 30. 0. searchBarOffset}}.
		{{#listMorph:selection:menu:icon: . #packageList . #packageSelection . #packageListMenu: . #packageIcon:}.{0. 0. 0.5. 0.6}. {0. searchBarOffset+3. 0. 0}}.
		
		{{#versionSearchField}.   {0.5. 0. 1. 0}.   {0. 30. 0. searchBarOffset}}.
		{{#listMorph:. #version}. {0.5. 0. 1. 0.6}. {0. searchBarOffset+3. 0. 0.}}.
		
		{{#textMorph:. #summary}. {0. 0.6. 1. 1}.   {0. 0. 0. 0.}}}
]
