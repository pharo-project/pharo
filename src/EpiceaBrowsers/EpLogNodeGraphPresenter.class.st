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
		'hiedraColumnController',
		'openSettingsButton',
		'refreshButton',
		'selectDirectoryButton',
		'hiedraCheckBox',
		'logNodesTable'
	],
	#category : #'EpiceaBrowsers-UI-Graph'
}

{ #category : #specs }
EpLogNodeGraphPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #logNodesTable;
		add:
			(SpBoxLayout newHorizontal
					add: #hiedraCheckBox;
					add: #refreshButton;
					add: #selectDirectoryButton;
					add: #openSettingsButton;
					yourself)
			expand: false
			fill: false
			padding: 0;
		yourself
]

{ #category : #'instance creation' }
EpLogNodeGraphPresenter class >> open [
	<script>
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

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeHiedraCheckBox [
	hiedraCheckBox
		help: 'Analyze and display connections between log files.';
		label: 'Link logs';
		state: false;
		whenChangedDo: [ self refresh ]
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
EpLogNodeGraphPresenter >> initializeLogNodesTable [
	self initializeHiedraController.

	logNodesTable
		beMultipleSelection;
		addColumn:
			((SpImageTableColumn evaluated: [ :item | hiedraColumnController cellMorphAtValue: item ])
				width: 50;
				yourself);
		addColumn: (SpStringTableColumn evaluated: #yourself)
]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeOpenSettingsButton [
	openSettingsButton
		label: 'Dashboard';
		action: [ EpDashboardPresenter open ]
]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeRefreshButton [
	refreshButton
		help: 'Refresh logs from this directory';
		icon: (self iconNamed: #smallUpdate);
		label: 'Refresh';
		action: [ self refresh ]
]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeSelectedDirectoryButton [
	selectDirectoryButton
		help: 'Select directory to browse logs (i.e. files with ombu extension)';
		icon: (self iconNamed: #openIcon);
		label: 'Directory';
		action: [ self selectDirectory ]
]

{ #category : #initialization }
EpLogNodeGraphPresenter >> initializeWidgets [
	hiedraCheckBox := self newCheckBox.
	selectDirectoryButton := self newButton.
	logNodesTable := self newTable.
	refreshButton := self newButton.
	openSettingsButton := self newButton.

	self initializeSelectedDirectoryButton.
	self initializeLogNodesTable.
	self initializeRefreshButton.
	self initializeHiedraCheckBox.
	self initializeOpenSettingsButton
]

{ #category : #refreshing }
EpLogNodeGraphPresenter >> isMonitorDirectory [

	^ self monitor sessionStore directory asFileReference = self directory asFileReference 
]

{ #category : #accessing }
EpLogNodeGraphPresenter >> monitor [
	
	^ monitor ifNil: [ EpMonitor current ]
]

{ #category : #accessing }
EpLogNodeGraphPresenter >> monitor: anEpMonitor [

	monitor := anEpMonitor

]

{ #category : #refreshing }
EpLogNodeGraphPresenter >> refresh [
	self refreshLogNodesTreePresenter
]

{ #category : #refreshing }
EpLogNodeGraphPresenter >> refreshLogNodesTreePresenter [
	| nodes |
	nodes := #().
	[ nodes := EpFileLogNode fromAllLogsIn: self directory.
	nodes := nodes sorted: [ :a :b | a modificationTime >= b modificationTime ] ]
		on: FileSystemError
		do: [ :error | self inform: error asString ].

	self isMonitorDirectory
		ifTrue: [ | currentGlobalName |
			currentGlobalName := self monitor sessionStore store globalName.
			nodes := nodes reject: [ :each | each globalName = currentGlobalName ].
			nodes := {(EpMonitorLogNode for: self monitor)} , nodes ].

	"Obtain the references between nodes only if the checkbox is selected since it's a potentially expensive process."
	hiedraCheckBox state ifTrue: [ nodes do: [ :node | node populateReferencedNodesWith: nodes ] displayingProgress: 'Analyzing ' , directory asString ].

	hiedraColumnController ruler: (HiRulerBuilder newRulerValues: nodes linksBlock: [ :node | node referencedNodes asArray ]).
	hiedraColumnController reset.
	logNodesTable items: nodes
]

{ #category : #accessing }
EpLogNodeGraphPresenter >> rowHeight [

	^ 23
]

{ #category : #private }
EpLogNodeGraphPresenter >> selectDirectory [
	(UIManager default chooseDirectoryFrom: self directory)
		ifNotNil: [ :selectedDirectory | self directory: selectedDirectory ]
]

{ #category : #accessing }
EpLogNodeGraphPresenter >> selectedLogNodes [
	^ logNodesTable selection selectedItems
]

{ #category : #'api-events' }
EpLogNodeGraphPresenter >> whenSelectionChangedDo: aBlock [
	logNodesTable whenSelectionChangedDo: aBlock
]
