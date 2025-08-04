Class {
	#name : 'SpIconPacksFetchPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'fetchButton',
		'searchIconTextPresenter',
		'searchButtonPresenter',
		'availableIconPacksPresenter',
		'fetchedIconPacks',
		'updateListButton',
		'iconsTablePresenter',
		'mainButtonBar',
		'applyButton',
		'closeButton',
		'repositoryListIconPacksPresenter',
		'logPresenter'
	],
	#classVars : [
		'JSONList'
	],
	#category : 'IconPacks',
	#package : 'IconPacks'
}

{ #category : 'accessing' }
SpIconPacksFetchPresenter class >> defaultPreferredExtent [

	^ 950 @ 650 
]

{ #category : 'accessing' }
SpIconPacksFetchPresenter class >> descriptionText [

	^ 'UI tool to query, download and set Icon Packs in the system'
]

{ #category : 'accessing' }
SpIconPacksFetchPresenter class >> fetchJsonList [

	^ ZnClient new
			get: 'https://api.github.com/repos/pharo-project/pharo-icon-packs/branches';
			contents
]

{ #category : 'accessing' }
SpIconPacksFetchPresenter class >> jsonList [
	"Answer a <String> representing a JSON with the icon-packs meta-information obtained from remote repository"

	^ JSONList
		ifNil: [ JSONList := self fetchJsonList ]
]

{ #category : 'accessing' }
SpIconPacksFetchPresenter class >> jsonList: aJSONString [
	"Set a <String> representing a JSON with the icon-packs meta-information obtained from remote repository"

	JSONList := aJSONString
]

{ #category : 'world menu' }
SpIconPacksFetchPresenter class >> menuCommandOn: aBuilder [
	<worldMenu>

	(aBuilder item: 'Icons Pack Manager')
		action: [ self open ];
		order: 37;
		parent: #Tools;
		icon: (self iconNamed: #configuration);
		help: self descriptionText
]

{ #category : 'instance creation' }
SpIconPacksFetchPresenter class >> open [
	<script>

	^ (self newApplication: StPharoApplication current) open
]

{ #category : 'class initialization' }
SpIconPacksFetchPresenter class >> reset [
	<script>

	JSONList := nil.
]

{ #category : 'settings' }
SpIconPacksFetchPresenter class >> settingsOn: aBuilder [
	<systemsettings>

	(aBuilder button: #open)
		parent: #appearance;
		order: 2;
		target: self;
		label: 'Icons Pack';
		buttonLabel: 'Open'
]

{ #category : 'private' }
SpIconPacksFetchPresenter >> checkAPILimitExceeded: jsonResponse [
	"Private - GitHub answer a JSON dictionary if there is any API error. Check jsonResponse and raise an <Error> if there was a API rate limit problem"
	
	(jsonResponse includesKey: 'message')
		ifFalse: [ ^ self ].
	((jsonResponse at: 'message') asLowercase beginsWith: 'api rate limit') 
		ifFalse: [ ^ self ].
	self inform: (jsonResponse at: 'message').
	self error
]

{ #category : 'layout' }
SpIconPacksFetchPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom 
		spacing: 5;
		add: (SpBoxLayout newLeftToRight 
			spacing: 5;
			add: (SpBoxLayout newTopToBottom 
				spacing: 5;			
				add: repositoryListIconPacksPresenter;
				add: (SpBoxLayout newLeftToRight 
					add: updateListButton;
					add: fetchButton;
					yourself) expand: false;
				add: availableIconPacksPresenter;
				yourself) expand: false;
			add: (SpBoxLayout newTopToBottom 
				spacing: 5;
				add: (SpBoxLayout newLeftToRight
					spacing: 5;
					add: searchIconTextPresenter;
					add: searchButtonPresenter expand: false;
					yourself) expand: false;			
				add: iconsTablePresenter;
				add: 'Log' expand: false;
				add: logPresenter height: 100;
				yourself);
			yourself);
		add: mainButtonBar expand: false;
		yourself
]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> downloadedIconPacksNames [
	"Answer a <Collection> of <ThemeIcons> each one representing a downloaded Icon Pack"

	^ ThemeIcons availablePacks
]

{ #category : 'accessing' }
SpIconPacksFetchPresenter >> fetchPacks [
	"Retrieve all packs available from repository. This fetch the **names** of the icon packs, not the icon resources. Answer a <Collection> of <ThemeIcon>"

	| jsonResponse fetchedJSONList |
	[ 
		fetchedJSONList := self class fetchJsonList.
		((jsonResponse := STON fromString: fetchedJSONList) isDictionary)
			ifTrue: [ self checkAPILimitExceeded: jsonResponse ]
			ifFalse: [ self jsonList: fetchedJSONList ]
	]
	on: Error
	do: [ :e | 
		e crTrace.
		^ { Smalltalk ui icons } ].

	^ jsonResponse
		  collect: [ :each |
			  | packName |
			  "Ensure we have just one instance of current pack"
			  (packName := each at: 'name') = Smalltalk ui icons name
				  ifTrue: [ Smalltalk ui icons ]
				  ifFalse: [ ThemeIcons named: packName ] ]
		  as: Array
]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> fetchSelectedIconPack: aThemeIcons [

	self updateLogPresenter: (self newLogEntryFromZipFile: aThemeIcons downloadFromUrl).
]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> fetchSelectedIconPacks [
	"Callback to make a remote request to download available icon packs"

	self selectedIconPacks 
		ifNotEmpty: [ : selectedIconsPck |
			selectedIconsPck 
				do: [ : iconPack | self fetchSelectedIconPack: iconPack ]
				displayingProgress: 'Downloading selected icon packs'.
			self updateDownloadedIconPacks ]
		ifEmpty: [ self inform: 'No selected icon(s) pack(s) to fetch' ]
	
]

{ #category : 'accessing' }
SpIconPacksFetchPresenter >> fetchedIconPacks [
	"Answer a <Collection> of <ThemeIcons>"
	
	^ fetchedIconPacks
		ifNil: [ fetchedIconPacks := self fetchPacks ]
]

{ #category : 'private' }
SpIconPacksFetchPresenter >> iconDimensions: aForm [

	^ String streamContents: [ : stream |
		stream 
			<< aForm width asString;
			<< $x;
			<< aForm height asString;
			<< $x;
			<< aForm depth asString ]

]

{ #category : 'initialization' }
SpIconPacksFetchPresenter >> initializeButtonsPresenters [

	applyButton := self newButton
		label: 'Set Current';
		action: [ self setCurrentIconPack ];
		disable;
		yourself.
		
	closeButton := self newButton
		label: 'Close';
		action: [ self delete ];
		yourself.

	mainButtonBar := self newButtonBar 
		add: applyButton;
		add: closeButton;
		yourself
]

{ #category : 'initialization' }
SpIconPacksFetchPresenter >> initializeIconPacksButtonsPresenters [

	updateListButton := self newButton
		label: 'Update';
		help: 'Query available packs from remote repository';
		icon: (self iconNamed: #smallUpdate);
		action: [ 
			self inform: 'Note that in case of error (no internet connection, for example) the remote Icons Pack list will display only the current icon set'.
			fetchedIconPacks := self fetchPacks.
			self updateLogPresenter: 'Fetched ' , fetchedIconPacks size asString , ' icon packs' ];
		yourself.

	fetchButton := self newButton
		label: 'Fetch';
		help: 'Download selected Icon Packs from remote repository';
		icon: (self iconNamed: #down);
		action: [ self fetchSelectedIconPacks ];
		yourself.


]

{ #category : 'initialization' }
SpIconPacksFetchPresenter >> initializeIconPacksPresenters [

	repositoryListIconPacksPresenter := (self instantiate: SpFilteringSelectableListPresenter)
		"headerTitle: 'Remote Icon Packs';"
		items: self fetchedIconPacks;
		display: [ : themeIcons | themeIcons name ];
		contextMenu: [  ];
		yourself.
	availableIconPacksPresenter := self newList
		headerTitle: 'Downloaded Icon Packs';
		items: self downloadedIconPacksNames;
		display: [ : iconsPack | iconsPack name ];
		whenSelectedItemChangedDo: [ : iconPack | self updateSelectedIconPack: iconPack ];
		"contextMenu: [ self availableIconsPackcontextMenuCommands ];"
		yourself.

]

{ #category : 'initialization' }
SpIconPacksFetchPresenter >> initializeIconTablePresenter [

	iconsTablePresenter := self newTable
		addColumn: (SpImageTableColumn new 
			width: 30;
			title: 'Icon';
			evaluated: [ : iconAssoc | iconAssoc value  ];
			beNotExpandable;
			yourself);
		addColumn: (SpStringTableColumn new  
			title: 'Name'; 
			evaluated: [ : iconAssoc | iconAssoc key asString ];
			beSortable;
			yourself);
		addColumn: (SpStringTableColumn new
			title: 'Dimensions (W x H x D)';
			evaluated: [ : iconAssoc | self iconDimensions: iconAssoc value ];
			beSortable;
			yourself);
		yourself

]

{ #category : 'initialization' }
SpIconPacksFetchPresenter >> initializeLogPresenter [

	logPresenter := self newList
]

{ #category : 'initialization' }
SpIconPacksFetchPresenter >> initializePresenters [ 

	self 
		initializeSearchPresenters;
		initializeIconPacksPresenters;
		initializeIconPacksButtonsPresenters;
		initializeButtonsPresenters;
		initializeIconTablePresenter;
		initializeLogPresenter
]

{ #category : 'initialization' }
SpIconPacksFetchPresenter >> initializeSearchPresenters [

	searchIconTextPresenter := self newSearchInput 
		placeholder: 'Search icon by name';
		whenTextChangedDo: [ :aString | self updateSearch: aString ];				
		yourself.
	searchButtonPresenter := self newButton
		iconName: #smallFind;	
		disable;			
		action: [ self updateSearch: searchIconTextPresenter text ];
		yourself.
]

{ #category : 'accessing' }
SpIconPacksFetchPresenter >> installedFetchPacks [
	"Answer a <Collection> with the receiver's icon packs which are installed in the system"

	^ self fetchedIconPacks select: [ : iconPack | iconPack hasIcons ]
]

{ #category : 'accessing' }
SpIconPacksFetchPresenter >> jsonList [
	"Answer a <String> representing a JSON with the icon-packs meta-information obtained from remote repository"
	
	^ self class jsonList
]

{ #category : 'accessing' }
SpIconPacksFetchPresenter >> jsonList: aJSONString [

	self class jsonList: aJSONString
]

{ #category : 'accessing' }
SpIconPacksFetchPresenter >> logPresenter [
	^ logPresenter
]

{ #category : 'private' }
SpIconPacksFetchPresenter >> newLogEntryFromThemeIcons: aThemeIcons [

	^ String streamContents: [ : stream |
		stream 
			<< 'Loaded ';
			<< aThemeIcons name;
			<< ': ';
			<< aThemeIcons icons size asString;
			<< ' icons.';
			<< ' Scale: ';
			<< aThemeIcons scale asString;
			<< '. URL: ';
			<< aThemeIcons url asString ]
]

{ #category : 'private' }
SpIconPacksFetchPresenter >> newLogEntryFromZipFile: aFileLocator [ 
	"Answer a <String> with aFileLocator information for a log reporter"

	^ aFileLocator isZipFile 
		ifFalse: [ 'Not a ZIP file: ' , (aFileLocator asString first: 10) ]
		ifTrue: [ String streamContents: [ : stream |
				stream 
					<< aFileLocator basename;
					<< ' (';
					<< aFileLocator humanReadableSize;
					<< ')' ] ]
]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> searchIcon: aString [ 
	"Update the receiver's icon entries with items matching aString"

	iconsTablePresenter items: (self searchedIconMatches: aString) reversed.

]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> searchedIconMatches: aString [ 
	"If there are filters applied, then search aString in the subset of the filtered entries, otherwise search in the cached log entries"
	
	^ iconsTablePresenter items
		select: [: iconAssoc | iconAssoc key asLowercase beginsWith: aString asLowercase ]


]

{ #category : 'accessing' }
SpIconPacksFetchPresenter >> selectedIconPacks [ 
	"Answer a <Collection> of <ThemeIcons> found in the repository"
	
	^ repositoryListIconPacksPresenter selectedItems
]

{ #category : 'initialization' }
SpIconPacksFetchPresenter >> setCurrentIconPack [
	
	| selectedIconsPack |

	(self confirm: 'Are you sure to replace the current icon pack?')
		ifFalse: [ ^ self ].
	selectedIconsPack := availableIconPacksPresenter selectedItem.
	selectedIconsPack beCurrent.
	self inform:
		'Current Icons Pack updated to: ' , selectedIconsPack name
]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> sortedIconsList: iconsCollection [

	^ iconsCollection associations asSortedCollection
]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> updateDownloadedIconPacks [

	availableIconPacksPresenter 
		items: self downloadedIconPacksNames
]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> updateIconsTable: aThemeIcons [

	aThemeIcons hasIcons
		ifFalse: [ aThemeIcons loadIconsFromUrl ].
	iconsTablePresenter 
		items: (self sortedIconsList: aThemeIcons icons).
	self updateLogPresenter: (self newLogEntryFromThemeIcons: aThemeIcons)

]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> updateLogPresenter: aString [
	"Private - Write aString to the receiver's log presenter pane"

	| logItems newLogItems |
	logItems := self logPresenter items asOrderedCollection.
	newLogItems := logItems add: aString; yourself. 
	self logPresenter items: newLogItems
]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> updateSearch: aString [

	iconsTablePresenter items: (self sortedIconsList: availableIconPacksPresenter selectedItem icons).
	aString 
		ifNotEmpty: [ searchButtonPresenter enable ]
		ifEmpty: [ searchButtonPresenter disable ].
	self searchIcon: aString
]

{ #category : 'callbacks' }
SpIconPacksFetchPresenter >> updateSelectedIconPack: anIconPack [

	anIconPack 
		ifNotNil: [ 
			applyButton enable.
			self updateIconsTable: anIconPack ]
		ifNil: [ 
			applyButton disable ]

]

{ #category : 'initialization' }
SpIconPacksFetchPresenter >> windowTitle [ 
	^ 'Icon Packs Manager'
]
