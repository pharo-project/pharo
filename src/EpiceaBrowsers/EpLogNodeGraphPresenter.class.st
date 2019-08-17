"
I am a browser for Epicea logs in a directory.

Usage:

self open.
"
Class {
	#name : #EpLogNodeGraphPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'directory',
		'monitor',
		'hiedraCheckBoxPresenter',
		'logNodesTreePresenter',
		'openSettingsButtonPresenter',
		'refreshButtonPresenter',
		'selectDirectoryButtonPresenter',
		'hiedraColumnController'
	],
	#category : #'EpiceaBrowsers-UI-Graph'
}

{ #category : #accessing }
EpLogNodeGraphPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #logNodesTreePresenter;
		add:
			(SpBoxLayout newHorizontal
				add: #hiedraCheckBoxPresenter;
				add: #refreshButtonPresenter;
				add: #selectDirectoryButtonPresenter;
				add: #openSettingsButtonPresenter;
				yourself)
			withConstraints: [ :constraints | constraints height: self buttonHeight - 4 ];
		yourself
]

{ #category : #'instance creation' }
EpLogNodeGraphPresenter class >> open [

	^ self new
		refresh;
		openWithSpec;
		yourself
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

	hiedraColumnController := HiColumnController new.	
	hiedraColumnController pageSize: 100.
	hiedraColumnController renderer
		rowHeight: self rowHeight;
		cellWidth: self rowHeight // 3;
		arrowSize: 2.0;
		nodeConnectionOffset: 2.5

]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeLogNodesTreePresenter [
	self initializeHiedraController.

	self logNodesTreePresenter
		addColumn:
			((SpImageTableColumn evaluated: [ :item | hiedraColumnController cellMorphAtValue: item ])
				width: 50;
				yourself);
		addColumn: (SpStringTableColumn evaluated: #yourself)
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
	hiedraCheckBoxPresenter := self newCheckBox.
	selectDirectoryButtonPresenter := self newButton.
	logNodesTreePresenter := self newTreeTable.
	refreshButtonPresenter := self newButton.
	openSettingsButtonPresenter := self newButton
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

	self isMonitorDirectory ifTrue: [
		| currentGlobalName |
		currentGlobalName := self monitor sessionStore store globalName.
		nodes := nodes reject: [:each | each globalName = currentGlobalName ].
		nodes := { EpMonitorLogNode for: self monitor }, nodes.
	].

	"Obtain the references between nodes only if the checkbox is selected since it's a potentially expensive process."
	self hiedraCheckBoxPresenter state ifTrue: [
		nodes 
			do: [ :node | node populateReferencedNodesWith: nodes ]
			displayingProgress: ('Analyzing ', directory asString) ].

	hiedraColumnController ruler: (HiRulerBuilder
		newRulerValues: nodes
		linksBlock: [ :node | node referencedNodes asArray ]).
	hiedraColumnController reset.

	self logNodesTreePresenter roots: nodes.
]

{ #category : #accessing }
EpLogNodeGraphPresenter >> rowHeight [

	^ 23
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
