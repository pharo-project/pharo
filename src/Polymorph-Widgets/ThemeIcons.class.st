"
I'm an icon pack who retrieve icons from a remote repository (https://github.com/pharo-project/pharo-icon-packs).

You should access this icons using #iconNamed: idiom: 

Smalltalk ui icons iconNamed: #add.

Iuse an override of #doesNotUnderstand: to provide compatibility with ""old way"" of providing icons: 

Smalltalk ui icon addIcon. 

Installation:
----------------
ThemeIconPack new 
	name: 'idea11';
	loadIconsFromUrl;
	beCurrent.

NOTE: ""name"" is the branch name in the repository.

"
Class {
	#name : #ThemeIcons,
	#superclass : #Object,
	#instVars : [
		'name',
		'url',
		'icons',
		'reportNotFound'
	],
	#classVars : [
		'Current'
	],
	#category : #'Polymorph-Widgets-Themes'
}

{ #category : #accessing }
ThemeIcons class >> availablePacks [
	"Retrieve all packs available in local disk"
	^ { self current }, (self destinationPath exists
		ifTrue: [  
			(self destinationPath allChildrenMatching: '*.zip') 
				select: [ :each | each base ~= self current name  ]
				thenCollect: [ :each | self named: each base ] ]
		ifFalse: [ #() ])
]

{ #category : #accessing }
ThemeIcons class >> baseUrl [
	^ 'https://github.com/pharo-project/pharo-icon-packs/archive' asUrl
]

{ #category : #settings }
ThemeIcons class >> createFetchButtonUpdating: listMorph [
	^ (Smalltalk ui theme 
		newButtonIn: self currentWorld
		for: self
		getState: nil 
		action: nil
		arguments: nil
		getEnabled: nil
		getLabel: nil
		help: 'Fetch icon pack from remote repository' translated)
		label: 'Fetch from remote';
		actionBlock: [ 
			self uiFetchPacks.
			listMorph update: #availablePacks ];
		yourself
]

{ #category : #settings }
ThemeIcons class >> createIconPackList [
	^ (Smalltalk ui theme 
		newDropListIn: self currentWorld
		for: self
		list: #availablePacks
		getSelected: #current
		setSelected: #current:
		getEnabled: nil
		useIndex: false
		help: nil)
		wrapSelector: #name;
		hResizing: #rigid;
		width: 120;
		yourself
]

{ #category : #settings }
ThemeIcons class >> createSettingRow [
	| list |
	^ Smalltalk ui theme 
		newRowIn: self currentWorld 
		for: {
			list := self createIconPackList.
			self createFetchButtonUpdating: list }
]

{ #category : #'instance creation' }
ThemeIcons class >> current [ 
	"WARNING: Direct access to this method is ill-adviced, use Smalltalk ui icons instead."
	^ Current ifNil: [ Current := self loadDefault ]
]

{ #category : #'instance creation' }
ThemeIcons class >> current: aPack [
	aPack hasIcons ifFalse: [ aPack loadIconsFromUrl ].
	Current := aPack.
	"Polymorph depends on Morphic, so coupling with WorldState is ok, the problem is with 
	 other tools (like Nautilus), that may need to refresh its icon caches"
	self currentWorld resetWorldMenu.
	SystemAnnouncer uniqueInstance announce: IconSetChanged
]

{ #category : #accessing }
ThemeIcons class >> destinationPath [
	^ 'icon-packs' asFileReference
]

{ #category : #accessing }
ThemeIcons class >> fetchPacks [
	"Retrieve all packs available from repository"
	| jsonList |
	
	[ jsonList := ZnClient new 
		get: 'https://api.github.com/repos/pharo-project/pharo-icon-packs/branches';
		contents ] 
	on: Error do: [ :e | 
		"in case of error (no inet connection, for example) I retrieve just current icon set"
		e crTrace.
		^ { self current } ].
	^ (STON fromString: jsonList) 
		collect: [ :each | | packName |
			"Ensure we have just one instance of current pack"
			packName := each at: 'name'.
			packName = self current name
				ifTrue: [ self current ]
				ifFalse: [ self named: packName ] ]
		as: Array
]

{ #category : #private }
ThemeIcons class >> loadDefault [
	^ self new 
	name: 'idea11';
	loadIconsFromUrl;
	yourself.
]

{ #category : #'instance creation' }
ThemeIcons class >> named: aString [ 
	^ self new name: aString
]

{ #category : #settings }
ThemeIcons class >> settingsOn: aBuilder [ 
	<systemsettings>

	(aBuilder setting: #current)
		parent: #appearance;
		order: 2;
		label: 'Icon Set';
		default: (ThemeIcons named: 'idea11');
		target: self;
		dialog: [ self createSettingRow ]
]

{ #category : #settings }
ThemeIcons class >> uiFetchPacks [
	| choosenPacks remotePacks |

	UIManager default 
		informUser: 'Retrieving available packs from remote repository' 
		during: [ remotePacks := self fetchPacks ].
		
	choosenPacks := (TickDialogWindow 
		itemsList: remotePacks
		itemsHeaderName: nil 
		wrapBlockOrSelector: #name 
		title: 'Remote available icon packs')
		chooseFromOwner: self currentWorld.
	choosenPacks ifNil: [ ^ self ].
	UIManager default informUserDuring: [ :bar | 
		choosenPacks do: [ :each |
			bar label: 'Retrieving "', each name, '" icon set from remote repository'.
			each downloadFromUrl ]] 	
]

{ #category : #settings }
ThemeIcons class >> wrapIconPack: aPack [
	^ aPack name
]

{ #category : #comparing }
ThemeIcons >> = anotherObject [

	^ self species = anotherObject species
		and: [ self name = anotherObject name ]
]

{ #category : #accessing }
ThemeIcons >> allIconNames [
	"Returns the names of all the available icons"
	^ self icons keys
]

{ #category : #accessing }
ThemeIcons >> beCurrent [
	self class current: self
]

{ #category : #accessing }
ThemeIcons >> beNotReportNotFound [
	reportNotFound := false
]

{ #category : #accessing }
ThemeIcons >> beReportNotFound [
	reportNotFound := true
]

{ #category : #accessing }
ThemeIcons >> blankIcon [
	^ self blankIconOfWidth: 16
]

{ #category : #utilities }
ThemeIcons >> blankIconOfWidth: aNumber [ 
	^ self icons
		at: ('blank-' , aNumber asString) asSymbol
		ifAbsentPut: [Form extent: aNumber @ 1 depth: 8]
]

{ #category : #private }
ThemeIcons >> defaultUrl [
	^ self class baseUrl / (self name, '.zip')
]

{ #category : #'reflective operations' }
ThemeIcons >> doesNotUnderstand: aMessage [
	"WARNING: This is "
	aMessage selector isUnary 
		ifTrue: [ ^ self iconNamed: aMessage selector ].	
	^ super doesNotUnderstand: aMessage
]

{ #category : #loading }
ThemeIcons >> downloadFromUrl [
	| zipArchive |
	self class destinationPath ensureCreateDirectory. 	
	zipArchive := self class destinationPath / (self name, '.zip').
	zipArchive exists 
		ifFalse: [ 
			ZnClient new
				url: self url;
				downloadTo: zipArchive ].

	^ zipArchive
]

{ #category : #utilities }
ThemeIcons >> form16x16FromContents: aByteArray [ 
	^ Form
	extent: 16@16
	depth: 32
	fromArray: aByteArray 
	offset: 0@0
]

{ #category : #testing }
ThemeIcons >> hasIcons [
	^ self icons notEmpty
]

{ #category : #comparing }
ThemeIcons >> hash [ 
	
	^ self name hash
]

{ #category : #accessing }
ThemeIcons >> iconNamed: aSymbol [
	^ (self 
		iconNamed: aSymbol 
		ifNone: [ 
			self isReportingNotFound 
				ifTrue: [ 
					self crTrace: (aSymbol, ' icon not found!').
					self notFoundIcon ]
				ifFalse: [ self blankIcon ]]) scaledByDisplayScaleFactor
]

{ #category : #accessing }
ThemeIcons >> iconNamed: aSymbol ifNone: aBlock [

	self icons at: aSymbol asSymbol ifPresent: [ :icon | ^ icon ].
	"Trying the old way"
	
	((aSymbol endsWith: 'Icon') or: [ (aSymbol endsWith: 'Form') ]) ifTrue: [ 
		self icons 
			at: (aSymbol allButLast: 4) asSymbol 
			ifPresent: [ :icon | ^ icon ] 
	].

	^ aBlock value
]

{ #category : #accessing }
ThemeIcons >> icons [ 
	^ icons ifNil: [ icons := Dictionary new: 0 ]
]

{ #category : #testing }
ThemeIcons >> isReportingNotFound [
	^ reportNotFound ifNil: [ reportNotFound := false ]
]

{ #category : #loading }
ThemeIcons >> loadIconsFromUrl [
	| newIcons zipArchive |
	
	newIcons := IdentityDictionary new.
	zipArchive := self downloadFromUrl.
	((FileSystem zip: zipArchive) open workingDirectory allChildrenMatching: '*.png')
		reject: [ :each | each base beginsWith: '.' ]
		thenDo: [ :each | 
			[ newIcons 	
				at: each base asSymbol
				put: (self readPNGFrom: each) ]
			on: Error do: [ :e | self crTrace: each fullName, ' not a PNG, skipping.'  ]].
	icons := newIcons.
]

{ #category : #accessing }
ThemeIcons >> name [
	^ name
]

{ #category : #accessing }
ThemeIcons >> name: aName [
	name := aName
]

{ #category : #accessing }
ThemeIcons >> notFoundIcon [
	^ self icons
		at: #notFoundIcon
		ifAbsentPut: [ Color red iconOrThumbnailOfSize: 16 ]
]

{ #category : #printing }
ThemeIcons >> printOn: stream [ 
	super printOn: stream.
	stream << $( << self name << $)
]

{ #category : #private }
ThemeIcons >> readPNGFrom: aReference [
	^ aReference binaryReadStreamDo: [ :stream | 
		PNGReadWriter formFromStream: stream ]
]

{ #category : #accessing }
ThemeIcons >> url [
	^ url ifNil: [ url := self defaultUrl ]
]

{ #category : #accessing }
ThemeIcons >> url: aStringOrUrl [
	url := aStringOrUrl ifNotNil: [ aStringOrUrl asUrl ]
]
