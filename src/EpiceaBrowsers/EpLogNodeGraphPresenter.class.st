"
I am a browser for Epicea logs in a directory.

Usage:

self open.
"
Class {
	#name : #EpLogNodeGraphPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'directory',
		'hiedraRulerController',
		'monitor',
		'hiedraCheckBoxPresenter',
		'logNodesTreePresenter',
		'openSettingsButtonPresenter',
		'refreshButtonPresenter',
		'selectDirectoryButtonPresenter'
	],
	#category : #'EpiceaBrowsers-UI-Graph'
}

{ #category : #'instance creation' }
EpLogNodeGraphPresenter class >> open [

	^ self new
		refresh;
		openWithSpec;
		yourself
]

{ #category : #accessing }
EpLogNodeGraphPresenter class >> spec [
	<spec: #default>

	^ SpecLayout composed newColumn: [:column |
		column add: #logNodesTreePresenter.

		column 
			newRow: [ :row |
				 row 
					add: #hiedraCheckBoxPresenter;
					add: #refreshButtonPresenter" width: self inputTextHeight";
					add: #selectDirectoryButtonPresenter" width: self inputTextHeight";
					add: #openSettingsButtonPresenter;
					yourself ]
			 height: self buttonHeight - 4 ].
]

{ #category : #accessing }
EpLogNodeGraphPresenter >> directory [

	^ directory ifNil: [ 
		directory := self monitor sessionStore baseLocator ]
]

{ #category : #accessing }
EpLogNodeGraphPresenter >> directory: aFileReference [

	directory := aFileReference.
	
	self refresh.
]

{ #category : #'accessing widgets' }
EpLogNodeGraphPresenter >> hiedraCheckBoxPresenter [
	^ hiedraCheckBoxPresenter
]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeHiedraCheckBoxPresenter [

	self hiedraCheckBoxPresenter
		help: 'Analyze and display connections between log files.';
		label: 'Link logs';
		state: false;
		whenChangedDo: [ self refresh ].

]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeHiedraController [

	hiedraRulerController := HiRulerController new.
	
	hiedraRulerController treeModel: self logNodesTreePresenter.
	self logNodesTreePresenter whenRootsChanged: [
		hiedraRulerController updateFromTree ].
	
	hiedraRulerController rulerBuilder
		rowSize: self rowSize;
		columnSize: self rowSize // 3.
	
	hiedraRulerController rulerBuilder addLinkBuilder: [ :linkBuilder |
		linkBuilder targetsBlock: [ :node | node referencedNodes ] ].

	"Set zero inset, else there is an empty gap cutting Hiedra links."
	self logNodesTreePresenter rowInset: 0.

]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeLogNodesTreePresenter [

	self initializeHiedraController.

	self logNodesTreePresenter columns:	{	
		hiedraRulerController newTreeColumnModel
			initialWidth: 50
			yourself.

		TreeColumnPresenter new
			displayBlock: [ :item | item content asMorph ];
			yourself.
	}.

]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeOpenSettingsButtonPresenter [

	self openSettingsButtonPresenter
		label: 'Dashboard';
		action: [ self openDashboard ].
]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializePresenter [
	super initializePresenter.
	self initializeSelectedDirectoryButtonPresenter.
	self initializeLogNodesTreePresenter.
	self initializeRefreshButtonPresenter.
	self initializeHiedraCheckBoxPresenter.
	self initializeOpenSettingsButtonPresenter
]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeRefreshButtonPresenter [

	self refreshButtonPresenter 
		help: 'Refresh logs from this directory';
		icon: (self iconNamed: #smallUpdate);
		label: 'Refresh';
		action: [ self refresh ]
]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeSelectedDirectoryButtonPresenter [
	
	self selectDirectoryButtonPresenter 
		help: 'Select directory to browse logs (i.e. files with ombu extension)';
		icon: (self iconNamed: #openIcon);
		label: 'Directory';
		action: [ self selectDirectory ]

]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeWidgets [

	self instantiatePresenters: {
		#hiedraCheckBoxPresenter -> #CheckBoxPresenter.
		#selectDirectoryButtonPresenter -> #ButtonPresenter.
		#logNodesTreePresenter -> #TreePresenter.
		#refreshButtonPresenter -> #ButtonPresenter.
		#openSettingsButtonPresenter -> #ButtonPresenter.
		}
]

{ #category : #refreshing }
EpLogNodeGraphPresenter >> isMonitorDirectory [

	^ self monitor sessionStore directory asFileReference = self directory asFileReference 
]

{ #category : #'accessing widgets' }
EpLogNodeGraphPresenter >> logNodesTreePresenter [
	^ logNodesTreePresenter
]

{ #category : #accessing }
EpLogNodeGraphPresenter >> monitor [
	
	^ monitor ifNil: [ EpMonitor current ]
]

{ #category : #accessing }
EpLogNodeGraphPresenter >> monitor: anEpMonitor [

	monitor := anEpMonitor

]

{ #category : #private }
EpLogNodeGraphPresenter >> openDashboard [

	EpDashboardPresenter open
]

{ #category : #'accessing widgets' }
EpLogNodeGraphPresenter >> openSettingsButtonPresenter [
	^ openSettingsButtonPresenter
]

{ #category : #refreshing }
EpLogNodeGraphPresenter >> refresh [
	self refreshLogNodesTreePresenter
]

{ #category : #'accessing widgets' }
EpLogNodeGraphPresenter >> refreshButtonPresenter [
	^ refreshButtonPresenter
]

{ #category : #refreshing }
EpLogNodeGraphPresenter >> refreshLogNodesTreePresenter [
	| nodes |
	nodes := #().
	[	nodes := EpFileLogNode fromAllLogsIn: self directory.
		nodes := nodes sorted: [ :a :b | a modificationTime >= b modificationTime ] ]
			on: FileSystemError 
			do: [ :error | self inform: error asString ].

	self flag: #fix. "hacky"
	self isMonitorDirectory ifTrue: [
		| currentGlobalName |
		currentGlobalName := self monitor sessionStore store globalName.
		nodes := nodes reject: [:each | each globalName = currentGlobalName ].
		nodes := { EpMonitorLogNode for: self monitor }, nodes.
	].

	self hiedraCheckBoxPresenter state ifTrue: [
		nodes 
			do: [ :node | node populateReferencedNodesWith: nodes ]
			displayingProgress: ('Analyzing ', directory asString)].

	self logNodesTreePresenter roots: nodes.

]

{ #category : #accessing }
EpLogNodeGraphPresenter >> rowSize [

	^ 23
]

{ #category : #private }
EpLogNodeGraphPresenter >> selectDirectory [

	(UIManager default chooseDirectoryFrom: self directory)
		ifNotNil: [ :selectedDirectory | self directory: selectedDirectory ]
]

{ #category : #'accessing widgets' }
EpLogNodeGraphPresenter >> selectDirectoryButtonPresenter [

	^ selectDirectoryButtonPresenter
]
