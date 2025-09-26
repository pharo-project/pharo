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
		'reportNotFound',
		'zipArchive'
	],
	#classVars : [
		'Current'
	],
	#category : 'Morphic-Base-Theme',
	#package : 'Morphic-Base',
	#tag : 'Theme'
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
ThemeIcons class >> defaultIconsPack [

	^ 'svgPack'
]

{ #category : 'accessing' }
ThemeIcons class >> destinationPath [
	^ FileLocator localDirectory / 'icon-packs'
]

{ #category : 'private' }
ThemeIcons class >> loadDefault [
	<script>
	^ self new
		  name: self defaultIconsPack;
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

	^ self iconsPerScale at: self scale ifAbsentPut: [ IdentityDictionary new ]
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

	self downloadFromUrl.
	^ self loadIconsFromZipArchiveUsingScale: newScale
]

{ #category : 'loading' }
ThemeIcons >> loadIconsFromZipArchiveUsingScale: newScale [
	| newIconsPerScale zipContent |

	newIconsPerScale := Dictionary new.
	zipContent := (FileSystem zip: zipArchive) open workingDirectory.

	"If it has the old icon pack structure we handle as they don't have multiple sizes"
	(zipContent allChildrenMatching: 'icons') 
		ifNotEmpty: [ 
			scale := 1.
			^ self loadIconsOldIconSetsFrom: zipContent ].

	(zipContent allChildrenMatching: 'png-scale*') do: [ :directory |
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

{ #category : 'loading' }
ThemeIcons >> loadIconsOldIconSetsFrom: zipContent [ 

	| newIcons |
	newIcons := IdentityDictionary new.
	
	(zipContent allChildrenMatching: '*.png')
		reject: [ :each | each base beginsWith: '.' ]
		thenDo: [ :each | 
			[ newIcons 	
				at: each base asSymbol
				put: (self readPNGFrom: each) ]
			on: Error do: [ :e | self crTrace: each fullName, ' not a PNG, skipping.'  ]].

	iconsPerScale := Dictionary new
	   at: 1.0 put: newIcons;
	   yourself
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
