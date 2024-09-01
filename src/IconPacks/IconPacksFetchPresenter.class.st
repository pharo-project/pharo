Class {
	#name : 'IconPacksFetchPresenter',
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
IconPacksFetchPresenter class >> descriptionText [

	^ 'UI tool to query, download and set Icon Packs in the system'
]

{ #category : 'accessing' }
IconPacksFetchPresenter class >> fetchJsonList [

	^ ZnClient new
			get: 'https://api.github.com/repos/pharo-project/pharo-icon-packs/branches';
			contents
]

{ #category : 'accessing' }
IconPacksFetchPresenter class >> jsonList [
	"Answer a <String> representing a JSON with the icon-packs meta-information obtained from remote repository"

	^ JSONList
		ifNil: [ JSONList := self fetchJsonList ]
]

{ #category : 'accessing' }
IconPacksFetchPresenter class >> jsonList: aJSONString [
	"Set a <String> representing a JSON with the icon-packs meta-information obtained from remote repository"

	JSONList := aJSONString
]

{ #category : 'world menu' }
IconPacksFetchPresenter class >> menuCommandOn: aBuilder [
	<worldMenu>

	(aBuilder item: 'Icons Pack Manager')
		action: [ self open ];
		order: 37;
		parent: #Tools;
		icon: (self iconNamed: #configuration);
		help: self descriptionText
]

{ #category : 'instance creation' }
IconPacksFetchPresenter class >> open [
	<script>

	^ self new open
]

{ #category : 'class initialization' }
IconPacksFetchPresenter class >> reset [
	<script>

	JSONList := nil.
]

{ #category : 'private' }
IconPacksFetchPresenter >> checkAPILimitExceeded: jsonResponse [

	(jsonResponse includesKey: 'message')
		ifFalse: [ ^ self ].
	((jsonResponse at: 'message') asLowercase beginsWith: 'api rate limit') 
		ifFalse: [ ^ self ].
	self inform: (jsonResponse at: 'message').
	self error
]

{ #category : 'layout' }
IconPacksFetchPresenter >> defaultLayout [

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
IconPacksFetchPresenter >> downloadedIconPacksNames [
	"Answer a <Collection> of <ThemeIcons> each one representing a downloaded Icon Pack"

	^ ThemeIcons availablePacks
]

{ #category : 'accessing' }
IconPacksFetchPresenter >> fetchPacks [
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
IconPacksFetchPresenter >> fetchSelectedIconPack: aThemeIcons [

	self updateLogPresenter: (self newLogEntryFromZipFile: aThemeIcons downloadFromUrl).
]

{ #category : 'callbacks' }
IconPacksFetchPresenter >> fetchSelectedIconPacks [
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
IconPacksFetchPresenter >> fetchedIconPacks [
	"Answer a <Collection> of <ThemeIcons>"
	
	^ fetchedIconPacks
		ifNil: [ fetchedIconPacks := self fetchPacks ]
]

{ #category : 'private' }
IconPacksFetchPresenter >> iconDimensions: aForm [

	^ String streamContents: [ : stream |
		stream 
			<< aForm width asString;
			<< $x;
			<< aForm height asString;
			<< $x;
			<< aForm depth asString ]

]

{ #category : 'initialization' }
IconPacksFetchPresenter >> initializeButtonsPresenters [

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
IconPacksFetchPresenter >> initializeIconPacksButtonsPresenters [

	updateListButton := self newButton
		label: 'Update';
		help: 'Query available packs from remote repository';
		icon: (self iconNamed: #smallUpdate);
		action: [ 
			self inform: 'Note that in case of error (no internet connection, for example) the remote Icons Pack list will display only the current icon set'.
			fetchedIconPacks := self fetchPacks ];
		yourself.

	fetchButton := self newButton
		label: 'Fetch';
		help: 'Download selected Icon Packs from remote repository';
		icon: (self iconNamed: #down);
		action: [ self fetchSelectedIconPacks ];
		yourself.


]

{ #category : 'initialization' }
IconPacksFetchPresenter >> initializeIconPacksPresenters [

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
		whenSelectedItemChangedDo: [ : iconPack | 
			applyButton enable.
			self updateIconsTable: iconPack ];
		"contextMenu: [ self availableIconsPackcontextMenuCommands ];"
		yourself.

]

{ #category : 'initialization' }
IconPacksFetchPresenter >> initializeIconTablePresenter [

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
IconPacksFetchPresenter >> initializeLogPresenter [

	logPresenter := self newList
]

{ #category : 'initialization' }
IconPacksFetchPresenter >> initializePresenters [ 

	self 
		initializeSearchPresenters;
		initializeIconPacksPresenters;
		initializeIconPacksButtonsPresenters;
		initializeButtonsPresenters;
		initializeIconTablePresenter;
		initializeLogPresenter
]

{ #category : 'initialization' }
IconPacksFetchPresenter >> initializeSearchPresenters [

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

{ #category : 'initialization' }
IconPacksFetchPresenter >> initializeWindow: aWindowPresenter [
	super initializeWindow: aWindowPresenter.
	
	aWindowPresenter 
		title: 'Icon Packs Manager';
		initialExtent: (950 @ 650) scaledByDisplayScaleFactor;
		centered
]

{ #category : 'accessing' }
IconPacksFetchPresenter >> installedFetchPacks [
	"Answer a <Collection> with the receiver's icon packs which are installed in the system"

	^ self fetchedIconPacks select: [ : iconPack | iconPack hasIcons ]
]

{ #category : 'accessing' }
IconPacksFetchPresenter >> jsonList [
	"Answer a <String> representing a JSON with the icon-packs meta-information obtained from remote repository"
	
	^ self class jsonList
]

{ #category : 'accessing' }
IconPacksFetchPresenter >> jsonList: aJSONString [

	self class jsonList: aJSONString
]

{ #category : 'accessing' }
IconPacksFetchPresenter >> logPresenter [
	^ logPresenter
]

{ #category : 'private' }
IconPacksFetchPresenter >> newLogEntryFromThemeIcons: aThemeIcons [

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
IconPacksFetchPresenter >> newLogEntryFromZipFile: aFileLocator [ 
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
IconPacksFetchPresenter >> searchIcon: aString [ 
	"Update the receiver's icon entries with items matching aString"

	iconsTablePresenter items: (self searchedIconMatches: aString) reversed.

]

{ #category : 'callbacks' }
IconPacksFetchPresenter >> searchedIconMatches: aString [ 
	"If there are filters applied, then search aString in the subset of the filtered entries, otherwise search in the cached log entries"
	
	^ iconsTablePresenter items
		select: [: iconAssoc | iconAssoc key asLowercase beginsWith: aString asLowercase ]


]

{ #category : 'accessing' }
IconPacksFetchPresenter >> selectedIconPacks [ 
	"Answer a <Collection> of <ThemeIcons> found in the repository"
	
	^ repositoryListIconPacksPresenter selectedItems
]

{ #category : 'initialization' }
IconPacksFetchPresenter >> setCurrentIconPack [
	
	| selectedIconsPack |

	(self confirm: 'Are you sure to replace the current icon pack?')
		ifFalse: [ ^ self ].
	selectedIconsPack := availableIconPacksPresenter selectedItem.
	selectedIconsPack beCurrent.
	self inform:
		'Current Icons Pack updated to: ' , selectedIconsPack name
]

{ #category : 'callbacks' }
IconPacksFetchPresenter >> updateDownloadedIconPacks [

	availableIconPacksPresenter 
		items: self downloadedIconPacksNames
]

{ #category : 'callbacks' }
IconPacksFetchPresenter >> updateIconsTable: aThemeIcons [

	aThemeIcons hasIcons
		ifFalse: [ aThemeIcons loadIconsFromUrl ].
	iconsTablePresenter 
		items: aThemeIcons icons associations asSortedCollection.
	self updateLogPresenter: (self newLogEntryFromThemeIcons: aThemeIcons)

]

{ #category : 'callbacks' }
IconPacksFetchPresenter >> updateLogPresenter: aString [

	| logItems newLogItems |
	logItems := self logPresenter items asOrderedCollection.
	newLogItems := logItems add: aString; yourself. 
	self logPresenter items: newLogItems
]

{ #category : 'callbacks' }
IconPacksFetchPresenter >> updateSearch: aString [

	aString 
		ifNotEmpty: [ searchButtonPresenter enable ]
		ifEmpty: [ searchButtonPresenter disable ].
	self searchIcon: aString
]
