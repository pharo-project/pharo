"
I am the main Spec2 UI presenter for the Quick Project Loader.

I display a list of commonly-used Metacello projects sorted by usage frequency (most-used first).
Each entry can be loaded with a single click or button press.

I also register:
- A World Menu item (<worldMenu>) under Tools
- A global keyboard shortcut (<keymap>) (Ctrl+Shift+L / Cmd+Shift+L)
- A startup action via SessionManager to optionally show the popup when Pharo starts
- System settings entry (<systemsettings>) to toggle auto-open on startup

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

	^ 450 @ 500
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

	^ (self newApplication: StPharoApplication current) open
]

{ #category : 'keymapping' }
QuickProjectLoaderPresenter class >> quickLoadShortcutOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #openQuickProjectLoader)
		category: #MorphCtrlNavigation
		default: $l shift ctrl asKeyCombination
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
		spacing: 5;
		add: projectList;
		add: (SpBoxLayout newLeftToRight
			spacing: 5;
			add: addButton;
			add: removeButton;
			add: loadButton;
			addLast: closeButton;
			yourself) expand: false;
		yourself
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initialize [

	repository := QuickProjectRepository default.
	super initialize
]

{ #category : 'initialization' }
QuickProjectLoaderPresenter >> initializeButtons [

	addButton := self newButton
		label: 'Add Project';
		icon: (self iconNamed: #add);
		help: 'Add a new Metacello project entry';
		action: [ self addProject ];
		yourself.
	removeButton := self newButton
		label: 'Remove';
		icon: (self iconNamed: #remove);
		help: 'Remove selected project entry';
		action: [ self removeSelectedProject ];
		disable;
		yourself.
	loadButton := self newButton
		label: 'Load';
		icon: (self iconNamed: #smallLoadProject);
		help: 'Load selected project using Metacello';
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
		headerTitle: 'Commonly Used Projects (Most Used First)';
		display: [ :entry | entry name , ' (loaded ' , entry loadCount printString , 'x) — ' , entry baselineName ];
		whenSelectionChangedDo: [ :selection | self updateButtonStates ];
		whenActivatedDo: [ :selection | self loadSelectedProject ];
		yourself.

	self initializeButtons.
	self refreshList
]

{ #category : 'actions' }
QuickProjectLoaderPresenter >> loadProject: anEntry [

	| confirmMessage |
	confirmMessage := 'Are you sure you want to load project ''', anEntry name, ''' (BaselineOf', anEntry baselineName, ')?'.
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

	^ 'Quick Project Loader'
]
