"
I am the main Spec2 UI presenter for the Quick Project Loader.

I display a list of commonly-used Metacello projects sorted by usage frequency (most-used first).
I am designed to be thin and positioned on the right side of the screen.

Features:
- Single-click or double-click to load projects
- Automatic inference of installed image projects ('Infer' button)
- Manual project entry ('Add Project' button)
- World menu integration (<worldMenu>)
- Global keyboard shortcut (<keymap>) (Cmd+Shift+L / Ctrl+Shift+L)
- Auto-startup via SessionManager and Pharo Settings (<systemsettings>)

## Usage

```
QuickProjectLoaderPresenter open.
```
"
Class {
	#name : 'QuickProjectLoaderPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'projectList',
		'addButton',
		'inferButton',
		'removeButton',
		'loadButton',
		'closeButton',
		'repository'
	],
	#classVars : [
		'ShowOnStartup'
	],
	#category : 'QuickProjectLoader-UI',
	#package : 'QuickProjectLoader',
	#tag : 'UI'
}

{ #category : 'accessing' }
QuickProjectLoaderPresenter class >> defaultPreferredExtent [

	^ 280 @ 450
]

{ #category : 'class initialization' }
QuickProjectLoaderPresenter class >> initialize [

	SessionManager default
		register: (ClassSessionHandler forClassNamed: self name)
		inCategory: SessionManager default userCategory
		atPriority: SessionManager default defaultPriority
]

{ #category : 'world menu' }
QuickProjectLoaderPresenter class >> menuCommandOn: aBuilder [
	<worldMenu>

	(aBuilder item: #QuickProjectLoader)
		action: [ self open ];
		order: 1;
		parent: #Tools;
		icon: (self iconNamed: #smallLoadProject);
		help: 'Open the Quick Project Loader to load commonly-used projects with one click'
]

{ #category : 'instance creation' }
QuickProjectLoaderPresenter class >> open [
	"Open the presenter and position it at the top-right corner of the World screen."

	| presenter window worldBounds windowWidth windowHeight topX topY morphWindow |
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
	^ window
]

{ #category : 'keymapping' }
QuickProjectLoaderPresenter class >> quickLoadShortcutOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #openQuickProjectLoader)
		category: #CmdWorldShortcutsCategory
		default: $l shift meta | $l shift ctrl
		do: [ :target :morph :event | self open ]
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
		self showOnStartup ifTrue: [
			self open ] ]
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
QuickProjectLoaderPresenter >> closeButton [

	^ closeButton
]

{ #category : 'layout' }
QuickProjectLoaderPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		spacing: 4;
		add: projectList;
		add: (SpBoxLayout newLeftToRight
			spacing: 3;
			add: addButton;
			add: inferButton;
			add: removeButton;
			yourself) expand: false;
		add: (SpBoxLayout newLeftToRight
			spacing: 3;
			add: loadButton;
			add: closeButton;
			yourself) expand: false;
		yourself
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> inferButton [

	^ inferButton
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> inferProjects [

	self repository inferProjects.
	self refreshList.
	self inform: 'Inferred projects from current image!'
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initialize [

	repository := QuickProjectRepository default.
	super initialize
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initializeButtons [

	addButton := self newButton
		label: 'Add';
		icon: (self iconNamed: #add);
		help: 'Add a new project entry by hand';
		action: [ self addProject ];
		yourself.
	inferButton := self newButton
		label: 'Infer';
		icon: (self iconNamed: #smallUpdate);
		help: 'Infer projects automatically from image';
		action: [ self inferProjects ];
		yourself.
	removeButton := self newButton
		label: 'Remove';
		icon: (self iconNamed: #remove);
		help: 'Remove selected project';
		action: [ self removeSelectedProject ];
		disable;
		yourself.
	loadButton := self newButton
		label: 'Load Selected';
		icon: (self iconNamed: #smallLoadProject);
		help: 'Load selected project';
		action: [ self loadSelectedProject ];
		disable;
		yourself.
	closeButton := self newButton
		label: 'Close';
		icon: (self iconNamed: #smallCancel);
		action: [ self delete ];
		yourself
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initializePresenters [

	projectList := self newList.
	projectList
		headerTitle: 'Common Projects (Most Used)';
		display: [ :entry | 
			| label |
			label := entry name.
			entry baselineName isNotEmpty ifTrue: [ label := label , ' (', entry baselineName, ')' ].
			label , ' [' , entry loadCount printString , 'x]' ];
		whenSelectionChangedDo: [ :selection | self updateButtonStates ];
		whenActivatedDo: [ :selection | self loadSelectedProject ];
		yourself.

	self initializeButtons.
	self refreshList
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
QuickProjectLoaderPresenter >> loadButton [

	^ loadButton
]

{ #category : 'accessing' }
QuickProjectLoaderPresenter >> projectList [

	^ projectList
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> refreshList [

	projectList items: self repository entries
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

{ #category : 'private' }
QuickProjectLoaderPresenter >> updateButtonStates [

	projectList selectedItem
		ifNotNil: [ 
			removeButton enable.
			loadButton enable ]
		ifNil: [ 
			removeButton disable.
			loadButton disable ]
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> windowTitle [

	^ 'Quick Loader'
]
