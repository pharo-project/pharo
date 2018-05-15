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
		newButtonIn: World
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
		newDropListIn: World
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
		newRowIn: World 
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
	World resetWorldMenu.
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
		e crLog.
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
		chooseFromOwner: World.
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

{ #category : #accessing }
ThemeIcons >> iconNamed: aSymbol [
	^ self 
		iconNamed: aSymbol 
		ifNone: [ 
			self isReportingNotFound 
				ifTrue: [ 
					self crLog: (aSymbol, ' icon not found!').
					self notFoundIcon ]
				ifFalse: [ self blankIcon ]]
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
			on: Error do: [ :e | self crLog: each fullName, ' not a PNG, skipping.'  ]].
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

{ #category : #'critiques icons' }
ThemeIcons >> repairIcon [
	^ icons
			at: #'repairIcon'
			ifAbsentPut: [ Form fromBinaryStream: self repairIconContents base64Decoded readStream ].
]

{ #category : #'critiques icons' }
ThemeIcons >> repairIconContents [
	
	^ 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACEElEQVR42mP4//8/AyUYTNja
2qJgu+Y1Eg6taw/YtaxxQpdDxlgNsGlapeDUteGWx4Qt/117Nv6ya12TSbQBlg0rNRw61j/x
mrz9v/+sPf99pu7879i5oXbSpElR5eXlN6urq7/n5ORM8/DwYABhFAPMapcZ2bSufe06Yet/
nxm7//vO3PPfY/L28unTp7MA8beampp/QDofphnFAKPKJbaWjas+OPZs/u8+ecd/jyk7/4fV
TriZmZnJBdTEXVlZ+Q/IngvTCBRjBLpkIdwA09plj2071v936t/y33nC1n/h5Z0fgBr+A3EA
ELNADZgDdKmvj48PG9CAKUCxl3ADzOtX7rDr3PjfvnvTX+vWdclAxepAnAjEQItyLkydOvU/
EP/LysqaVVBQsLmuru5/WVnZBbgBJjXLG61a1/4xb1odBwoPkDOBmgXj4uIeTJ48+T/QRhD+
B9T0FRiYf4GGvQDyXeEG6JcucjOuXhYBiw2QAbGxsWdBNsM0Aw08GBoayoU1EGEak5OTW6uq
qj4C8QeoRjCOjo6+mZqaWgQ0JBgUsDgNAAbMa6CGPiDugWmOiIg4HB8fLwYNk+dAvAdmCIYB
paWl54GamKFR1Qc08BnMNqAmVSB+Co0dsCEYBgA1CUFpDiBeWFFR8Qg56UJjB2ZIO9a80NHR
IQcM6etA2x8DnV6Dnv6BGtWAeCmKCyjBAMWc9s7ZKakLAAAAAElFTkSuQmCC'
]

{ #category : #'critiques icons' }
ThemeIcons >> smallInformationIcon [

	^ self smallInfoIcon 
]

{ #category : #'critiques icons' }
ThemeIcons >> smallQAIcon [
	^ icons
			at: #'smallQAIcon'
			ifAbsentPut: [ Form fromBinaryStream: self smallQAIconContents base64Decoded readStream ].
]

{ #category : #'critiques icons' }
ThemeIcons >> smallQAIconContents [

	^ '
iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAABCRpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIgogICAgICAgICAgICB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iCiAgICAgICAgICAgIHhtbG5zOnhtcD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wLyI+CiAgICAgICAgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDx0aWZmOkNvbXByZXNzaW9uPjU8L3RpZmY6Q29tcHJlc3Npb24+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjcyPC90aWZmOlhSZXNvbHV0aW9uPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICAgICA8dGlmZjpZUmVzb2x1dGlvbj43MjwvdGlmZjpZUmVzb2x1dGlvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjE2PC9leGlmOlBpeGVsWERpbWVuc2lvbj4KICAgICAgICAgPGV4aWY6Q29sb3JTcGFjZT4xPC9leGlmOkNvbG9yU3BhY2U+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj4xNjwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgICAgIDxkYzpzdWJqZWN0PgogICAgICAgICAgICA8cmRmOkJhZy8+CiAgICAgICAgIDwvZGM6c3ViamVjdD4KICAgICAgICAgPHhtcDpNb2RpZnlEYXRlPjIwMTUtMDMtMjRUMTE6MDM6OTc8L3htcDpNb2RpZnlEYXRlPgogICAgICAgICA8eG1wOkNyZWF0b3JUb29sPlBpeGVsbWF0b3IgMy4zLjE8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgPC9yZGY6RGVzY3JpcHRpb24+CiAgIDwvcmRmOlJERj4KPC94OnhtcG1ldGE+Ct6L88gAAAHISURBVDgRxZK/S8NQEMeTNAk1oJvS6iAIgoOCIG4OLi4O/gtCEX+MOqlIW0sVcXIUCk66KLi5KLiIo+DiIohr0E1QpGkaP9/iK7VKVw8ud+/uvnffdy+W9d9itxMoFouZVCo1niTJKLpOPkH38B+w9+TDVkyzAYkpgMv1en3GcZw+1KrVao1a13Ut4tIX27YvCR7m8/lbJW2A/dgCgByFrkAq/ksAW2oWx3EEoyP8bZfCO9/3MwStarX6xtknkTbTWxsBsqIosmjkwXaJmjkHJ0MwJDlP8Rg6AYMDGP2ioem6GhjDMusC1EUXuNNFy7TVUqnUAyBnmKghLCvUnGEz2C2YjLhMe8pms1rMD6HxMbmcgpqMf1ooFJZNEbt7oMGNQ6D5EiYpy8RmXMtDrvQxQoN7hjxrB0NhGM6ahLHaie4rwReDSZOTLZfLg5gBm7vqR3lFNz3Pu2ah3fiL6AqgxmgxQD9plKfpObaP/C6xae1AdHs5VHjGdxI+rDyzPM4NBpg0dfsssCifGkdP734/SSyaaKB6ErGmtouGIV1ogh+DTYnBGoENtPnuatZJ1JwaB+yO/oMKgQ8Aw6jXCWhyYCL8xyAITr4Ak1nwl6ws9bYAAAAASUVORK5CYII='
]

{ #category : #'critiques icons' }
ThemeIcons >> thumbsDownIcon [
	^ icons
			at: #thumbsDownIcon
			ifAbsentPut: [ Form fromBinaryStream: self tumbsDownIconContents base64Decoded readStream ].
]

{ #category : #'critiques icons' }
ThemeIcons >> thumbsUpIcon [
	^ icons
			at: #thumbsUpIcon
			ifAbsentPut: [ Form fromBinaryStream: self thumbsUpIconContents base64Decoded readStream ].
]

{ #category : #'critiques icons' }
ThemeIcons >> thumbsUpIconContents [

	^ 'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAABY0lEQVQokY2RTStFcRDGfzP/cw5ObhZigZXr5dpZ2dvwAaxsRBaSIgt2vgJlI+/KQllZ8QGUjZQUpZAUsSCpK7nn/sfiiJu8TU1NM/P0zDwP/BVrTVmWWlaZr4tL2/onMJEeyrUfF7eWtoNfQQu5FpyN8mp5In//P8bphgpccZYKrcfYpO/i6n+MmXicMu0m7xNUZr6O5VvQYnMbTneBaowEs2HwB4TAC08MnZ0Ly60ZQt8A+sJj8Z5yaSTUDYQcib0/JODNpzfKM973BnimMB0DCsRuDrMuHDle7fMCbyAfelTiTQLEOjGJUCKwKiDzwVQaVlKJNitm66i8D8QD/kfBUlUEk1DBjkh+3/0GfaIEySFFrnECYgKU/ehuIFCwB5zbVwYuHxG2CQRMnhB28NzhBCJJDQslTbgBmWDg+Db1caWphjjMkvdXDJ7eMJetJZJ2zHUgNglsobbFc2GPkctbgDetr3ZCqo6gPwAAAABJRU5ErkJggg=='
]

{ #category : #'critiques icons' }
ThemeIcons >> tumbsDownIconContents [

	^ 'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAABmklEQVQokX3Qv2pUURDH8e/vnHPv/jGwGyO4NotIFJSILIuFpLLyAQRBsEln4ROkEQufwEax0d4HsLETFEVNhGiKKGKiJs26YeMS7t17xuJqkexmf82Bgc+cmRH/snf5dMssWbRoN710fQQPouyFnP/YqJ/a7O1unWsEvzDI9WZ2Zf2bAPrds7cDdtdLrWiQmVF1IhoU2GBkvBRsN5Ow1MvzJ3Pvvyyp1z3Tlrm1RJrJzTicRJAb3w1ezzh3Yy/G7WEIC85MV2puMgKI5TMCKDBSqVXNY8d506U42YzFgCAIilediVosa9OIF3ivcn8nlp2D5wIJ0ASSSmDaAjZj5IdDO0XU0yCUSOzLqMaytaKVI6QSWWQVC7eamf/ZT/7ca6ZVz7v13zJQrzN/vgaNTJaZ6YLEYycqwG4+YnFudWPt8CRBYHzY+PS/sNOd71fAanIMivhwEgJwYzsZdxreV4cxvirq/v5R5zpwj/7F9qyF9HPFuXRYjK6dWPn69ih44Meinh5z4uS+xWfT0Bg83u78MrHssEfTEMBf2XCor7p8mm8AAAAASUVORK5CYII='
]

{ #category : #accessing }
ThemeIcons >> url [
	^ url ifNil: [ url := self defaultUrl ]
]

{ #category : #accessing }
ThemeIcons >> url: aStringOrUrl [
	url := aStringOrUrl ifNotNil: [ aStringOrUrl asUrl ]
]
