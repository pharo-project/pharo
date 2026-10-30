"
I am the small panel of the Quick Project Loader: the projects you loaded recently, one click away from being loaded again in this image.

I open by myself once in a fresh image, in the top-right corner, without taking the keyboard focus, and only if there is something to propose. Closing me is enough to dismiss me. After that I come back with the shortcut (Option+P / Cmd+Shift+P) or from the Tools menu.

Nothing happens at startup besides a flag check: the history is read when I am about to open, and the network is only used when you pick a project. Loads run in a background process, so I never block the image.

## Usage

```
QuickProjectLoaderPresenter toggle.
```
"
Class {
	#name : 'QuickProjectLoaderPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'header',
		'searchInput',
		'list',
		'rowsMorph',
		'items',
		'repository',
		'loadingEntry'
	],
	#classVars : [
		'CurrentWindow',
		'Offered',
		'ShowOnStartup'
	],
	#category : 'QuickProjectLoader-UI',
	#package : 'QuickProjectLoader',
	#tag : 'UI'
}

{ #category : 'instance creation' }
QuickProjectLoaderPresenter class >> closeWindow [

	CurrentWindow ifNotNil: [
		CurrentWindow close.
		CurrentWindow := nil ]
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> defaultPreferredExtent [

	^ self extentForRows: 5
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> extentForRows: anInteger [
	"Only as tall as what it shows, up to a list that still fits in the corner of the screen."

	| rows |
	rows := (anInteger max: 1) min: self maxVisibleRows.
	^ 396 @ (rows * QuickProjectRowMorph rowHeight + 60
		+ (anInteger > self maxVisibleRows ifTrue: [ 32 ] ifFalse: [ 0 ]))
]

{ #category : 'private' }
QuickProjectLoaderPresenter class >> hasDisplay [
	"False when the image runs from the command line or in a CI job."

	^ UIManager default isKindOf: MorphicUIManager
]

{ #category : 'class initialization' }
QuickProjectLoaderPresenter class >> initialize [

	KMRepository default setAsGlobalCategory: #QuickProjectLoaderCategory.
	self registerWorldShortcut.
	SessionManager default registerUserClassNamed: self name
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> listSideMargin [

	^ 10
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> maxBranchesInMenu [
	"Beyond this a menu stops being quicker than typing the name in Other..."

	^ 20
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> maxVisibleRows [
	"Past this the list scrolls and a search field earns its place."

	^ 6
]

{ #category : 'world menu' }
QuickProjectLoaderPresenter class >> menuCommandOn: aBuilder [
	<worldMenu>

	(aBuilder item: #QuickProjectLoader)
		label: 'Quick Project Loader';
		action: [ self toggle ];
		order: 1;
		parent: #Tools;
		icon: (self iconNamed: #smallLoadProject);
		help: 'Open or close the Quick Project Loader'
]

{ #category : 'instance creation' }
QuickProjectLoaderPresenter class >> offer [
	"Propose the recent projects without disturbing whatever the user is doing."

	| previous window |
	Offered := true.
	QuickProjectRepository default syncWithDisk.
	QuickProjectRepository default visibleEntries ifEmpty: [ ^ self ].
	previous := SystemWindow topWindow.
	window := self open.
	previous ifNotNil: [ previous activate ].
	^ window
]

{ #category : 'instance creation' }
QuickProjectLoaderPresenter class >> open [
	"Open the panel in the top-right corner of the World."

	| window world extent |
	self closeWindow.

	window := (self newApplication: StPharoApplication current) open.

	world := self currentWorld bounds.
	extent := self extentForRows: window presenter items size.
	window window
		extent: extent;
		position: ((world right - extent x - 8) max: 0) @ (world top + 35).

	CurrentWindow := window.
	^ window
]

{ #category : 'keymapping' }
QuickProjectLoaderPresenter class >> quickLoadShortcutOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #toggleQuickProjectLoader)
		category: #QuickProjectLoaderCategory
		default: $p alt | $p command shift mac | $p ctrl shift win | $p ctrl shift unix
		do: [ :target :morph :event | self toggle ]
]

{ #category : 'keymapping' }
QuickProjectLoaderPresenter class >> registerWorldShortcut [
	<script>
	"Binds the toggle shortcut directly to the World Morph so it works everywhere on macOS. The binding is saved with the image."

	self currentWorld ifNotNil: [ :world |
		world bindKeyCombination: $p alt toAction: [ self toggle ].
		world bindKeyCombination: $p meta shift toAction: [ self toggle ].
		world bindKeyCombination: $o meta shift toAction: [ self toggle ] ]
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> resetOffered [
	<script>

	Offered := nil
]

{ #category : 'settings' }
QuickProjectLoaderPresenter class >> settingsOn: aBuilder [
	<systemsettings>

	(aBuilder setting: #showOnStartup)
		label: 'Show Quick Project Loader in new images';
		parent: #pharoSystem;
		default: true;
		target: self;
		description: 'If true, the Quick Project Loader proposes your recent projects the first time a new image starts'
]

{ #category : 'testing' }
QuickProjectLoaderPresenter class >> shouldOfferOnStartup [
	"This runs on the startup path: it must stay a few flag checks, with no file access."

	^ Offered isNil and: [ self showOnStartup and: [ self hasDisplay ] ]
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> showOnStartup [

	^ ShowOnStartup ifNil: [ ShowOnStartup := true ]
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> showOnStartup: aBoolean [

	ShowOnStartup := aBoolean
]

{ #category : 'system startup' }
QuickProjectLoaderPresenter class >> shutDown: isQuitting [
	"Nothing to do on shutdown."
]

{ #category : 'system startup' }
QuickProjectLoaderPresenter class >> startUp: isImageStarting [

	(isImageStarting and: [ self shouldOfferOnStartup ])
		ifTrue: [ UIManager default defer: [ self offer ] ]
]

{ #category : 'instance creation' }
QuickProjectLoaderPresenter class >> toggle [
	"Toggle the panel: open if closed, close if open."

	(CurrentWindow notNil and: [ CurrentWindow isOpen ]) ifTrue: [
		self closeWindow.
		^ self ].
	self open presenter takeSearchFocus
]

{ #category : 'private' }
QuickProjectLoaderPresenter >> actionLabelFor: anEntry [

	anEntry = loadingEntry ifTrue: [ ^ 'Loading...' ].
	^ anEntry isLoadedInImage
		ifTrue: [ 'Reload' ]
		ifFalse: [ 'Load' ]
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> addProject [

	| dialog presenter |
	presenter := self instantiate: QuickProjectAddPresenter.
	dialog := presenter openModal.
	dialog isOk ifTrue: [
		self repository addEntry: presenter createEntry.
		self refreshList ]
]

{ #category : 'private' }
QuickProjectLoaderPresenter >> branchMenuFor: anEntry names: aCollection [

	| menu |
	menu := MenuMorph new.
	menu addTitle: 'Load branch'.
	(aCollection first: (aCollection size min: self class maxBranchesInMenu)) do: [ :each |
		menu add: each target: self selector: #loadBranch:of: argumentList: { each. anEntry }.
		each = anEntry effectiveBranch ifTrue: [ menu lastItem icon: (self iconNamed: #smallOk) ] ].
	menu addLine.
	menu add: 'Other...' target: self selector: #askBranchAndLoad: argument: anEntry.
	^ menu
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> chooseBranchOf: anEntry [
	"Asking GitHub can take a moment, so the menu comes back from a background process."

	[
		| names |
		names := anEntry fetchBranches.
		UIManager default defer: [
			names
				ifEmpty: [ self askBranchAndLoad: anEntry ]
				ifNotEmpty: [ (self branchMenuFor: anEntry names: names) popUpInWorld ] ] ]
		forkAt: Processor userBackgroundPriority
		named: 'Quick Project Loader: branches of ' , anEntry name
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> copyScriptOf: anEntry [

	Clipboard clipboardText: anEntry metacelloLoadScript.
	self notify: 'Metacello script copied'
]

{ #category : 'layout' }
QuickProjectLoaderPresenter >> defaultLayout [

	| layout |
	layout := SpBoxLayout newTopToBottom yourself.
	layout add: header expand: false.
	self repository visibleEntries size > self class maxVisibleRows
		ifTrue: [ layout add: searchInput expand: false ].
	^ layout
		add: list;
		yourself
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> disableStartupOffer [

	self class showOnStartup: false.
	self window close
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> filterProjects: aString [

	| query |
	query := aString trimBoth asLowercase.
	items := query
		ifEmpty: [ self repository visibleEntries ]
		ifNotEmpty: [
			self repository visibleEntries select: [ :entry |
				(entry name asLowercase includesSubstring: query)
					or: [ entry repositoryUrl asLowercase includesSubstring: query ] ] ].
	self rebuildRows
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initialize [

	repository := QuickProjectRepository default.
	repository syncWithDisk.
	super initialize
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initializeHeader [

	header := self newMorph
		morph: (QuickProjectHeaderMorph
			title: 'Recent projects'
			buttons: {
				QuickProjectIconButtonMorph
					icon: (self iconNamed: #add)
					help: 'Add a project'
					action: [ self addProject ].
				QuickProjectIconButtonMorph
					icon: (self iconNamed: #glamorousClose)
					help: 'Close'
					action: [ self close ] });
		yourself
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initializeList [

	| pane |
	rowsMorph := Morph new
		color: Color transparent;
		borderWidth: 0;
		changeTableLayout;
		layoutInset: (6 @ 0 corner: 6 @ 6);
		cellInset: 2;
		hResizing: #rigid;
		vResizing: #shrinkWrap;
		yourself.
	pane := GeneralScrollPaneMorph new
		color: Color transparent;
		borderWidth: 0;
		hScrollbarShowNever;
		vScrollbarShowWhenNeeded;
		scrollTarget: rowsMorph;
		yourself.
	list := self newMorph
		morph: pane;
		yourself
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initializePresenters [

	searchInput := self newSearchInput
		placeholder: 'Search projects...';
		whenTextChangedDo: [ :text | self filterProjects: text ];
		yourself.

	self initializeHeader.
	self initializeList.
	self refreshList
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initializeWindow: aWindowPresenter [

	super initializeWindow: aWindowPresenter.
	aWindowPresenter withoutDecorations
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> items [

	^ items
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> askBranchAndLoad: anEntry [

	(UIManager default
		request: 'Branch, tag or commit (empty for the default branch)'
		initialAnswer: anEntry effectiveBranch) ifNotNil: [ :answer |
			self loadBranch: answer trimBoth of: anEntry ]
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> loadBranch: aString of: anEntry [
	"The choice is remembered, so the row keeps loading this branch."

	loadingEntry ifNotNil: [ ^ self ].
	anEntry branch: aString.
	self repository save.
	self loadProject: anEntry
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> loadProject: anEntry [
	"Download in the background. Metacello reports the finished load to the repository, which is what brings the project to the top of the list."

	loadingEntry ifNotNil: [ ^ self ].
	loadingEntry := anEntry.
	self refreshList.
	[ [ [
		anEntry load.
		self notify: anEntry name , ' loaded' ]
			on: Error
			do: [ :ex |
				self notify: 'Could not load ' , anEntry name , ': ' , ex messageText.
				ex pass ] ]
		ensure: [
			loadingEntry := nil.
			UIManager default defer: [ self refreshList ] ] ]
		forkAt: Processor userBackgroundPriority
		named: 'Quick Project Loader: ' , anEntry name
]

{ #category : 'private' }
QuickProjectLoaderPresenter >> notify: aString [

	UIManager default inform: aString
]

{ #category : 'private' }
QuickProjectLoaderPresenter >> rebuildRows [

	rowsMorph removeAllMorphs.
	items do: [ :entry | rowsMorph addMorphBack: (self rowFor: entry) ].
	items ifEmpty: [
		rowsMorph addMorphBack: ((StringMorph contents: 'Projects you load will show up here')
			color: self theme ghostTextColor;
			yourself) ].
	self updateScrolling
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> refreshList [

	self filterProjects: searchInput text
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> removeProject: anEntry [

	self repository removeEntry: anEntry.
	self refreshList
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> repository [

	^ repository ifNil: [ repository := QuickProjectRepository default ]
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> repository: aRepository [

	repository := aRepository.
	self refreshList
]

{ #category : 'private' }
QuickProjectLoaderPresenter >> rowFor: anEntry [

	^ (QuickProjectRowMorph
		entry: anEntry
		action: (self actionLabelFor: anEntry)
		onLoad: [ self loadProject: anEntry ]
		onMenu: [ (self rowMenuFor: anEntry) popUpInWorld ])
		branchBlock: [ self chooseBranchOf: anEntry ];
		yourself
]

{ #category : 'private' }
QuickProjectLoaderPresenter >> rowMenuFor: anEntry [

	^ MenuMorph new
		add: 'Copy Metacello Script' target: self selector: #copyScriptOf: argument: anEntry;
		add: 'Remove from List' target: self selector: #removeProject: argument: anEntry;
		addLine;
		add: 'Don''t Show in New Images' target: self selector: #disableStartupOffer;
		yourself
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> searchInput [

	^ searchInput
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> takeSearchFocus [

	searchInput takeKeyboardFocus
]

{ #category : 'private' }
QuickProjectLoaderPresenter >> updateScrolling [
	"The panel is as tall as its rows, so only a list past the maximum height ever needs a scrollbar."

	| pane scrolls |
	pane := list morph.
	scrolls := items size > self class maxVisibleRows.
	scrolls
		ifTrue: [ pane vScrollbarShowWhenNeeded ]
		ifFalse: [ pane vScrollbarShowNever ].
	"Only a scrolling list gives up width: otherwise the rows keep the same margins as the header."
	rowsMorph width: self class defaultPreferredExtent x - self class listSideMargin
		- (scrolls ifTrue: [ pane scrollBarThickness + 4 ] ifFalse: [ 0 ])
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> windowTitle [

	^ 'Recent Projects'
]
