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
	#name : 'ThemeIcons',
	#superclass : 'Object',
	#instVars : [
		'name',
		'url',
		'iconsPerScale',
		'scale',
		'formSetsCache',
		'reportNotFound'
	],
	#classVars : [
		'Current'
	],
	#category : 'Polymorph-Widgets-Themes',
	#package : 'Polymorph-Widgets',
	#tag : 'Themes'
}

{ #category : 'accessing' }
ThemeIcons class >> availablePacks [
	"Retrieve all packs available in local disk"
	^ { self current }, (self destinationPath exists
		ifTrue: [
			(self destinationPath allChildrenMatching: '*.zip')
				select: [ :each | each base ~= self current name  ]
				thenCollect: [ :each | self named: each base ] ]
		ifFalse: [ #() ])
]

{ #category : 'accessing' }
ThemeIcons class >> baseUrl [
	^ 'https://github.com/pharo-project/pharo-icon-packs/archive' asUrl
]

{ #category : 'settings' }
ThemeIcons class >> createFetchButtonUpdating: listMorph [
	^ (Smalltalk ui theme
		newButtonIn: self
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

{ #category : 'settings' }
ThemeIcons class >> createIconPackList [
	^ (Smalltalk ui theme
		newDropListIn: Morph new
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

{ #category : 'settings' }
ThemeIcons class >> createSettingRow [
	| list |
	^ Smalltalk ui theme
		newRowIn: self
		for: {
			list := self createIconPackList.
			self createFetchButtonUpdating: list }
]

{ #category : 'instance creation' }
ThemeIcons class >> current [
	"WARNING: Direct access to this method is ill-adviced, use Smalltalk ui icons instead."
	^ Current ifNil: [ Current := self loadDefault ]
]

{ #category : 'instance creation' }
ThemeIcons class >> current: aPack [

	aPack hasIcons ifFalse: [ aPack loadIconsFromUrl ].
	Current := aPack.
	self codeSupportAnnouncer announce: IconSetChanged
]

{ #category : 'accessing' }
ThemeIcons class >> destinationPath [
	^ FileLocator localDirectory / 'icon-packs'
]

{ #category : 'accessing' }
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

{ #category : 'private' }
ThemeIcons class >> loadDefault [
	^ self new
	name: 'svgPack';
	loadIconsFromUrl;
	yourself
]

{ #category : 'instance creation' }
ThemeIcons class >> named: aString [
	^ self new name: aString
]

{ #category : 'class initialization' }
ThemeIcons class >> reset [
	<script>

	Current := nil
]

{ #category : 'settings' }
ThemeIcons class >> settingsOn: aBuilder [
	<systemsettings>

	(aBuilder setting: #current)
		parent: #appearance;
		order: 2;
		label: 'Icon Set';
		default: (ThemeIcons named: 'svgPack');
		target: self;
		dialog: [ self createSettingRow ]
]

{ #category : 'settings' }
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

{ #category : 'settings' }
ThemeIcons class >> wrapIconPack: aPack [
	^ aPack name
]

{ #category : 'comparing' }
ThemeIcons >> = anotherObject [

	^ self species = anotherObject species
		and: [ self name = anotherObject name ]
]

{ #category : 'accessing' }
ThemeIcons >> allIconNames [
	"Returns the names of all the available icons"
	^ self icons keys
]

{ #category : 'private' }
ThemeIcons >> basicIconFormSetNamed: aSymbol [

	| displayScaleFactor |

	^ ((formSetsCache ifNil: [ formSetsCache := Dictionary new ])
		at: (displayScaleFactor := self currentWorld displayScaleFactor)
		ifAbsentPut: [ IdentityDictionary new ])
			at: aSymbol ifAbsentPut: [
				self icons at: aSymbol
					ifPresent: [ :form |
						| allForms scaledForm |
						allForms := OrderedCollection new.
						iconsPerScale do: [ :icons | icons at: aSymbol ifPresent: [ :otherForm | allForms add: otherForm ] ].
						allForms add: (scaledForm := form scaledToSize: form extent * (displayScaleFactor / self scale)).
						FormSet extent: scaledForm extent depth: scaledForm depth forms: (Array withAll: allForms) ]
					ifAbsent: [ ^ nil ] ]
]

{ #category : 'accessing' }
ThemeIcons >> beCurrent [
	self class current: self
]

{ #category : 'accessing' }
ThemeIcons >> beNotReportNotFound [
	reportNotFound := false
]

{ #category : 'accessing' }
ThemeIcons >> beReportNotFound [
	reportNotFound := true
]

{ #category : 'accessing' }
ThemeIcons >> blankIcon [

	^ self blankIconFormSet asForm
]

{ #category : 'accessing' }
ThemeIcons >> blankIconFormSet [

	^ self iconFormSetNamed: #blank
]

{ #category : 'private' }
ThemeIcons >> defaultUrl [
	^ self class baseUrl / (self name, '.zip')
]

{ #category : 'reflective operations' }
ThemeIcons >> doesNotUnderstand: aMessage [
	"WARNING: This is "
	aMessage selector isUnary
		ifTrue: [ ^ self iconNamed: aMessage selector ].
	^ super doesNotUnderstand: aMessage
]

{ #category : 'loading' }
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

{ #category : 'utilities' }
ThemeIcons >> form16x16FromContents: aByteArray [
	^ Form
	extent: 16@16
	depth: 32
	fromArray: aByteArray
	offset: 0@0
]

{ #category : 'testing' }
ThemeIcons >> hasIcons [
	^ self icons notEmpty
]

{ #category : 'comparing' }
ThemeIcons >> hash [

	^ self name hash
]

{ #category : 'accessing' }
ThemeIcons >> iconFormSetNamed: aSymbol [

	^ (self
		iconFormSetNamed: aSymbol
		ifNone: [
			self isReportingNotFound
				ifTrue: [
					self crTrace: (aSymbol, ' icon not found!').
					self notFoundIconFormSet ]
				ifFalse: [
					aSymbol ~= #blank
						ifTrue: [ self blankIconFormSet ]
						ifFalse: [ FormSet form: (Form extent: 0@0) ]]])
]

{ #category : 'accessing' }
ThemeIcons >> iconFormSetNamed: aSymbol ifNone: aBlock [

	(self basicIconFormSetNamed: aSymbol asSymbol) ifNotNil: [ :icon | ^ icon ].
	"Trying the old way"

	
	((aSymbol endsWith: 'Icon') or: [ (aSymbol endsWith: 'Form') ]) ifTrue: [
		(self basicIconFormSetNamed: (aSymbol allButLast: 4) asSymbol)
			ifNotNil: [ :icon | 
				('Using old icon name, please rename ', aSymbol printString) traceCr.
				^ icon ]
	].

	^ aBlock value
]

{ #category : 'accessing' }
ThemeIcons >> iconNamed: aSymbol [

	^ (self iconFormSetNamed: aSymbol) asForm
]

{ #category : 'accessing' }
ThemeIcons >> iconNamed: aSymbol ifNone: aBlock [

	^ (self iconFormSetNamed: aSymbol ifNone: [ ^ aBlock value ]) asForm
]

{ #category : 'accessing' }
ThemeIcons >> icons [

	^ self iconsPerScale at: scale ifAbsentPut: [ IdentityDictionary new ]
]

{ #category : 'accessing' }
ThemeIcons >> iconsPerScale [

	^ iconsPerScale ifNil: [ iconsPerScale := Dictionary new ]
]

{ #category : 'testing' }
ThemeIcons >> isReportingNotFound [
	^ reportNotFound ifNil: [ reportNotFound := false ]
]

{ #category : 'loading' }
ThemeIcons >> loadIconsFromUrl [

	^ self loadIconsFromUrlUsingScale: 1
]

{ #category : 'loading' }
ThemeIcons >> loadIconsFromUrlUsingScale: newScale [
	| newIconsPerScale zipArchive |

	newIconsPerScale := Dictionary new.
	zipArchive := self downloadFromUrl.

	((FileSystem zip: zipArchive) open workingDirectory allChildrenMatching: 'png-scale*') do: [ :directory |
		| newIcons |
		newIconsPerScale
			at: (Float readFrom: (directory basename copyFrom: 10 to: directory basename size))
			put: (newIcons := IdentityDictionary new).
		(directory allChildrenMatching: '*.png')
			reject: [ :each | each base beginsWith: '.' ]
			thenDo: [ :each |
				[ newIcons
					at: each base asSymbol
					put: (self readPNGFrom: each) ]
				on: Error do: [ :e | self crTrace: ('{1} not a PNG, skipping.' format: { each fullName }) ] ] ].
	newIconsPerScale keysAndValuesDo: [ :iconsScale :icons |
		icons at: #notFound ifAbsentPut: [ Color red iconOrThumbnailOfSize: 16 * iconsScale ] ].
	iconsPerScale := newIconsPerScale.
	scale := newScale
]

{ #category : 'accessing' }
ThemeIcons >> name [
	^ name
]

{ #category : 'accessing' }
ThemeIcons >> name: aName [
	name := aName
]

{ #category : 'accessing' }
ThemeIcons >> notFoundIcon [

	^ self notFoundIconFormSet asForm
]

{ #category : 'accessing' }
ThemeIcons >> notFoundIconFormSet [

	^ self iconFormSetNamed: #notFound ifNone: [ FormSet form: (Form extent: 0@0) ]
]

{ #category : 'printing' }
ThemeIcons >> printOn: stream [
	super printOn: stream.
	stream << $( << self name << $)
]

{ #category : 'private' }
ThemeIcons >> readPNGFrom: aReference [
	^ aReference binaryReadStreamDo: [ :stream |
		PNGReadWriter formFromStream: stream ]
]

{ #category : 'accessing' }
ThemeIcons >> scale [
	^ scale ifNil: [ scale := 1 ]
]

{ #category : 'accessing' }
ThemeIcons >> url [
	^ url ifNil: [ url := self defaultUrl ]
]

{ #category : 'accessing' }
ThemeIcons >> url: aStringOrUrl [
	url := aStringOrUrl ifNotNil: [ aStringOrUrl asUrl ]
]
