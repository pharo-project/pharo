"
I am the main Spec2 UI presenter for the Quick Project Loader.

I display a modern list/table of commonly-used Metacello projects sorted by usage frequency (most-used first).
I am designed to be thin (320px wide), tall (600px high), and positioned on the right side of the screen.

Features:
- Global shortcut bound directly to World (Option+P / Cmd+Shift+P) to toggle (open & close) the window from anywhere in Pharo
- Live search/filtering bar
- Loaded vs Unloaded status distinction
- Auto-infer projects from image on startup & open
- Table view with icons, project names, baseline subtitles, and context menu
- Single/double-click loading
- Manual project entry ('Add Project' button)
- World menu integration (<worldMenu>)
- Auto-startup via SessionManager and Pharo Settings (<systemsettings>)

## Usage

```
QuickProjectLoaderPresenter toggle.
```
"
Class {
	#name : 'QuickProjectLoaderPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'searchInput',
		'projectList',
		'addButton',
		'removeButton',
		'repository'
	],
	#classVars : [
		'CurrentWindow',
		'ShowOnStartup'
	],
	#category : 'QuickProjectLoader-UI',
	#package : 'QuickProjectLoader',
	#tag : 'UI'
}

{ #category : 'instance creation' }
QuickProjectLoaderPresenter class >> closeWindow [

	CurrentWindow ifNotNil: [ 
		[ CurrentWindow close ] on: Error do: [].
		CurrentWindow := nil ]
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> defaultPreferredExtent [

	^ 320 @ 600
]

{ #category : 'class initialization' }
QuickProjectLoaderPresenter class >> initialize [

	KMRepository default setAsGlobalCategory: #QuickProjectLoaderCategory.
	self registerWorldShortcut.

	SessionManager default
		register: (ClassSessionHandler forClassNamed: self name)
		inCategory: SessionManager default userCategory
		atPriority: SessionManager default defaultPriority
]

{ #category : 'world menu' }
QuickProjectLoaderPresenter class >> menuCommandOn: aBuilder [
	<worldMenu>

	(aBuilder item: #QuickProjectLoader)
		action: [ self toggle ];
		order: 1;
		parent: #Tools;
		icon: (self iconNamed: #smallLoadProject);
		help: 'Open or close the Quick Project Loader'
]

{ #category : 'instance creation' }
QuickProjectLoaderPresenter class >> open [
	"Open the presenter, auto-infer projects, and position it at the top-right corner of the World screen."

	| presenter window worldBounds windowWidth windowHeight topX topY morphWindow |
	self closeWindow.

	presenter := self newApplication: StPharoApplication current.
	window := presenter open.

	worldBounds := self currentWorld bounds.
	windowWidth := self defaultPreferredExtent x.
	windowHeight := self defaultPreferredExtent y.
	topX := (worldBounds right - windowWidth - 5) max: 0.
	topY := (worldBounds top + 35) max: 0.

	(window respondsTo: #window) ifTrue: [ 
		morphWindow := window window.
		morphWindow ifNotNil: [ 
			morphWindow extent: windowWidth @ windowHeight.
			morphWindow position: topX @ topY ] ].

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

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> recentImagesWindowSize [

	^ QuickProjectRepository recentImagesWindowSize
]

{ #category : 'keymapping' }
QuickProjectLoaderPresenter class >> registerWorldShortcut [
	<script>
	"Binds the toggle shortcut directly to the World Morph so it works everywhere on macOS."

	[ 
		self currentWorld ifNotNil: [ :w |
			w bindKeyCombination: $p alt toAction: [ self toggle ].
			w bindKeyCombination: $p meta shift toAction: [ self toggle ].
			w bindKeyCombination: $o meta shift toAction: [ self toggle ] ]
	] on: Error do: [ :ex | "Ignore if World is not ready" ]
]

{ #category : 'settings' }
QuickProjectLoaderPresenter class >> settingsOn: aBuilder [
	<systemsettings>

	(aBuilder setting: #showOnStartup)
		label: 'Show Quick Project Loader on startup';
		parent: #pharoSystem;
		default: true;
		target: self;
		description: 'If true, the Quick Project Loader popup appears when a new image starts'
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

	isImageStarting ifTrue: [
		QuickProjectRepository default currentSessionMarker: DateAndTime now.
		self registerWorldShortcut.
		self showOnStartup ifTrue: [
			self open ] ]
]

{ #category : 'instance creation' }
QuickProjectLoaderPresenter class >> toggle [
	"Toggle the presenter window: open if closed, close if open."

	(CurrentWindow notNil and: [ 
		(CurrentWindow respondsTo: #isBuilt) and: [ CurrentWindow isBuilt ] ]) ifTrue: [ 
			self closeWindow.
			^ self ].
	self open
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> addButton [

	^ addButton
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> addProject [

	| dialog presenter |
	presenter := QuickProjectAddPresenter new.
	dialog := presenter openModal.
	dialog isOk ifTrue: [
		presenter createEntry ifNotNil: [ :entry |
			self repository addEntry: entry.
			self refreshList ] ]
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> copySelectedScript [

	projectList selectedItem ifNotNil: [ :entry |
		Clipboard clipboardText: entry metacelloLoadScript.
		self inform: 'Metacello load script copied to clipboard!' ]
]

{ #category : 'layout' }
QuickProjectLoaderPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		spacing: 5;
		add: searchInput expand: false;
		add: projectList;
		add: (SpBoxLayout newLeftToRight
			spacing: 4;
			add: addButton;
			add: removeButton;
			yourself) expand: false;
		yourself
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> filterProjects: aString [

	| query filtered |
	query := aString trimBoth asLowercase.
	query ifEmpty: [
			self refreshList.
			^ self ].
	filtered := self repository visibleEntries select: [ :entry |
			            (entry name asLowercase includesSubstring: query) or: [
				            (entry baselineName asLowercase includesSubstring: query) or: [
					            entry repositoryUrl asLowercase includesSubstring: query ] ] ].
	projectList items: filtered
]

{ #category : 'private' }
QuickProjectLoaderPresenter >> iconFor: anEntry [

	^ anEntry isLoadedInImage
		ifTrue: [ self iconNamed: #smallOk ]
		ifFalse: [ nil ]
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initialize [

	repository := QuickProjectRepository default.
	repository inferProjects.
	super initialize
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initializeButtons [

	addButton := self newButton
		label: 'Add';
		icon: (self iconNamed: #add);
		help: 'Add a new project entry manually';
		action: [ self addProject ];
		yourself.
	removeButton := self newButton
		label: 'Remove';
		icon: (self iconNamed: #remove);
		help: 'Remove selected project';
		action: [ self removeSelectedProject ];
		disable;
		yourself.
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initializePresenters [

	searchInput := self newSearchInput
		placeholder: 'Search projects...';
		whenTextChangedDo: [ :text | self filterProjects: text ];
		yourself.

	projectList := self newTable.
	projectList
		addColumn: (SpImageTableColumn new
			width: 22;
			evaluated: [ :entry | self iconFor: entry ];
			yourself);
		addColumn: (SpImageTableColumn new
			title: 'Project';
			evaluated: [ :entry | self labelMorphFor: entry ];
			sortFunction: [ :entry | entry name ] ascending;
			yourself);
		contextMenu: [ 
			self newMenu
				addItem: [ :item | 
					item 
						name: 'Load Project';
						icon: (self iconNamed: #smallLoadProject);
						action: [ self loadSelectedProject ] ];
				addItem: [ :item | 
					item 
						name: 'Copy Metacello Script';
						icon: (self iconNamed: #smallCopy);
						action: [ self copySelectedScript ] ];
				addItem: [ :item | 
					item 
						name: 'Remove';
						icon: (self iconNamed: #remove);
						action: [ self removeSelectedProject ] ];
				yourself ];
		whenSelectionChangedDo: [ :selection | self updateButtonStates ];
		whenActivatedDo: [ :selection | self loadSelectedProject ];
		yourself.

	self initializeButtons.
	self refreshList.

	self whenDisplayDo: [ projectList takeKeyboardFocus ]
]

{ #category : 'private' }
QuickProjectLoaderPresenter >> labelMorphFor: anEntry [

	| label |
	label := anEntry name asStringMorph.
	anEntry isLoadedInImage ifFalse: [
		label disableLabel
	].
	^ label
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> loadProject: anEntry [

	| confirmMessage |
	confirmMessage := 'Load project ''', anEntry name, '''?'.
	(self confirm: confirmMessage) ifFalse: [ ^ self ].

	self repository recordLoadOf: anEntry.
	self refreshList.

	[ 
		anEntry load.
		self inform: 'Project ''', anEntry name, ''' loaded successfully!'
	] on: Error do: [ :ex |
		self inform: 'Error loading project: ', ex messageText.
		ex pass ]
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> loadSelectedProject [

	projectList selectedItem ifNotNil: [ :entry |
		self loadProject: entry ]
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> projectList [

	^ projectList
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> refreshList [

	projectList items: self repository visibleEntries
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> removeButton [

	^ removeButton
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> removeSelectedProject [

	projectList selectedItem ifNotNil: [ :entry |
		(self confirm: 'Remove project ''', entry name, ''' from list?') ifTrue: [
			self repository removeEntry: entry.
			self refreshList ] ]
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

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> searchInput [

	^ searchInput
]

{ #category : 'private' }
QuickProjectLoaderPresenter >> updateButtonStates [

	projectList selectedItem
		ifNotNil: [ removeButton enable ]
		ifNil: [ removeButton disable ]
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> windowTitle [

	^ 'Quick Loader'
]
