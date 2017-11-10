"
I am a browser for Epicea logs in a directory.

Usage:

self open.
"
Class {
	#name : #EpLogNodeGraphModel,
	#superclass : #ComposableModel,
	#instVars : [
		'refreshButtonModel',
		'directory',
		'hiedraRulerController',
		'selectDirectoryButtonModel',
		'logNodesTreeModel',
		'directoryTextModel',
		'monitor',
		'openSettingsButtonModel',
		'hiedraCheckBoxModel'
	],
	#category : #EpiceaBrowsers-UI-Graph
}

{ #category : #'instance creation' }
EpLogNodeGraphModel class >> open [

	^ self new
		refresh;
		openWithSpec;
		yourself
]

{ #category : #accessing }
EpLogNodeGraphModel class >> spec [
	<spec: #default>

	^ SpecLayout composed newColumn: [:column |
		column add: #logNodesTreeModel.

		column 
			newRow: [ :row |
				 row 
					add: #hiedraCheckBoxModel;
					add: #refreshButtonModel" width: self inputTextHeight";
					add: #selectDirectoryButtonModel" width: self inputTextHeight";
					add: #openSettingsButtonModel;
					yourself ]
			 height: self buttonHeight - 4 ].
]

{ #category : #accessing }
EpLogNodeGraphModel >> directory [

	^ directory ifNil: [ 
		directory := self monitor sessionStore baseLocator ]
]

{ #category : #accessing }
EpLogNodeGraphModel >> directory: aFileReference [

	directory := aFileReference.
	
	self refresh.
]

{ #category : #'accessing widgets' }
EpLogNodeGraphModel >> hiedraCheckBoxModel [
	^ hiedraCheckBoxModel
]

{ #category : #initialization }
EpLogNodeGraphModel >> initializeHiedraCheckBoxModel [

	hiedraCheckBoxModel
		help: 'Analyze and display connections between log files.';
		label: 'Link logs';
		state: false;
		whenChangedDo: [ self refresh ].

]

{ #category : #initialization }
EpLogNodeGraphModel >> initializeHiedraController [

	hiedraRulerController := HiRulerController new.
	
	hiedraRulerController treeModel: logNodesTreeModel.
	logNodesTreeModel whenRootsChanged: [
		hiedraRulerController updateFromTree ].
	
	hiedraRulerController rulerBuilder
		rowSize: self rowSize;
		columnSize: self rowSize // 3.
	
	hiedraRulerController rulerBuilder addLinkBuilder: [ :linkBuilder |
		linkBuilder targetsBlock: [ :node | node referencedNodes ] ].

	"Set zero inset, else there is an empty gap cutting Hiedra links."
	logNodesTreeModel rowInset: 0.

]

{ #category : #initialization }
EpLogNodeGraphModel >> initializeLogNodesTreeModel [

	self initializeHiedraController.

	logNodesTreeModel columns:	{	
		hiedraRulerController newTreeColumnModel
			initialWidth: 50
			yourself.

		TreeColumnModel new
			displayBlock: [ :item | item content asMorph ];
			yourself.
	}.

]

{ #category : #initialization }
EpLogNodeGraphModel >> initializeOpenSettingsButtonModel [

	openSettingsButtonModel
		label: 'Dashboard';
		action: [ self openDashboard ].
]

{ #category : #initialization }
EpLogNodeGraphModel >> initializePresenter [ 
	super initializePresenter.
	
	self initializeSelectedDirectoryButtonModel.
	self initializeLogNodesTreeModel.
	self initializeRefreshButtonModel.
	self initializeHiedraCheckBoxModel.
	self initializeOpenSettingsButtonModel.
]

{ #category : #initialization }
EpLogNodeGraphModel >> initializeRefreshButtonModel [

	refreshButtonModel 
		help: 'Refresh logs from this directory';
		icon: (self iconNamed: #smallUpdate);
		label: 'Refresh';
		action: [ self refresh ]
]

{ #category : #initialization }
EpLogNodeGraphModel >> initializeSelectedDirectoryButtonModel [
	
	selectDirectoryButtonModel 
		help: 'Select directory to browse logs (i.e. files with ombu extension)';
		icon: (self iconNamed: #openIcon);
		label: 'Directory';
		action: [ self selectDirectory ]

]

{ #category : #initialization }
EpLogNodeGraphModel >> initializeWidgets [

	self instantiateModels: {
		#hiedraCheckBoxModel -> #CheckBoxModel.
		#selectDirectoryButtonModel -> #ButtonModel.
		#logNodesTreeModel -> #TreeModel.
		#refreshButtonModel -> #ButtonModel.
		#openSettingsButtonModel -> #ButtonModel.
		}
]

{ #category : #refreshing }
EpLogNodeGraphModel >> isMonitorDirectory [

	^ self monitor sessionStore directory asFileReference = self directory asFileReference 
]

{ #category : #'accessing widgets' }
EpLogNodeGraphModel >> logNodesTreeModel [
	^ logNodesTreeModel
]

{ #category : #accessing }
EpLogNodeGraphModel >> monitor [
	
	^ monitor ifNil: [ EpMonitor current ]
]

{ #category : #accessing }
EpLogNodeGraphModel >> monitor: anEpMonitor [

	monitor := anEpMonitor

]

{ #category : #private }
EpLogNodeGraphModel >> openDashboard [

	EpDashboardModel open
]

{ #category : #'accessing widgets' }
EpLogNodeGraphModel >> openSettingsButtonModel [
	^ openSettingsButtonModel
]

{ #category : #refreshing }
EpLogNodeGraphModel >> refresh [

	self refreshLogNodesTreeModel.

]

{ #category : #'accessing widgets' }
EpLogNodeGraphModel >> refreshButtonModel [
	^ refreshButtonModel
]

{ #category : #refreshing }
EpLogNodeGraphModel >> refreshLogNodesTreeModel [

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

	hiedraCheckBoxModel state ifTrue: [
		nodes 
			do: [ :node | node populateReferencedNodesWith: nodes ]
			displayingProgress: ('Analyzing ', directory asString)].

	logNodesTreeModel roots: nodes.

]

{ #category : #accessing }
EpLogNodeGraphModel >> rowSize [

	^ 23
]

{ #category : #private }
EpLogNodeGraphModel >> selectDirectory [

	(UIManager default chooseDirectoryFrom: self directory)
		ifNotNil: [ :selectedDirectory | self directory: selectedDirectory ]
]

{ #category : #'accessing widgets' }
EpLogNodeGraphModel >> selectDirectoryButtonModel [

	^ selectDirectoryButtonModel
]
