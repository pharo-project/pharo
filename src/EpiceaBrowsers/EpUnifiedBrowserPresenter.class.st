"
I'm a Spec model for the main tool to browse Epicea logs.

Open with:
self open
"
Class {
	#name : 'EpUnifiedBrowserPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'refreshDeferrer',
		'logPresenter',
		'logNodesPresenter',
		'toolbarPresenter'
	],
	#category : 'EpiceaBrowsers-UI-Unified',
	#package : 'EpiceaBrowsers',
	#tag : 'UI-Unified'
}

{ #category : 'accessing' }
EpUnifiedBrowserPresenter class >> defaultPreferredExtent [

	^ (950 @ 650)
]

{ #category : 'instance creation' }
EpUnifiedBrowserPresenter class >> open [

	<script>
	^ (self newApplication: StPharoApplication current)
		  refresh;
		  open;
		  yourself
]

{ #category : 'accessing' }
EpUnifiedBrowserPresenter class >> taskbarIconName [

	^ #recoverLostChanges
]

{ #category : 'accessing' }
EpUnifiedBrowserPresenter class >> worldMenuItemOn: aBuilder [
	<worldMenu>

	(aBuilder item: 'Code Changes')
		parent: #Changes;
		action: [ self open ];
		iconName: self taskbarIconName;
		help: 'Browse recorded change logs during from Pharo coding sessions and replay changes.';
		order: 1
]

{ #category : 'layout' }
EpUnifiedBrowserPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		add: toolbarPresenter expand: false;
		add: (SpPanedLayout newLeftToRight
			positionOfSlider: 35 percent;
			add: logNodesPresenter;
			add: logPresenter;
			yourself);
		yourself
]

{ #category : 'refreshing' }
EpUnifiedBrowserPresenter >> freshLogFromSelection: selection [
	^ selection selectedItems
		ifEmpty: [ EpLog newNull ]
		ifNotEmpty: [ :items |
			| entries entriesPerLog entryPerOriginalReference newLog |
			entries := OrderedCollection new.
			entriesPerLog := IdentityDictionary new.
			entryPerOriginalReference := Dictionary new.
			"Needed to reverse the selected logs to have the expected order (as user)."
			items reversed reverseDo: [ :item |
				entries addAll: ((entriesPerLog at: item log put: item log entries)
					do: [ :entry |
						entryPerOriginalReference at: (item log referenceTo: entry) put: entry ];
					yourself) ].
			newLog := EpLog newWithEntries: entries.
			entriesPerLog keysAndValuesDo: [ :log :logEntries |
				logEntries do: [ :entry |
					log triggererReferenceOf: entry
						ifPresent: [ :originalTriggerReference |
							log triggererReferenceOf: entry put: (entryPerOriginalReference
								at: originalTriggerReference
								ifPresent: [ :triggerEntry | newLog referenceTo: triggerEntry ]
								ifAbsent: [ newLog nullReference ]) ]
						ifAbsent: [ ] ] ].
			newLog ]
]

{ #category : 'initialization' }
EpUnifiedBrowserPresenter >> initializePresenter [

	super initializePresenter.
	logNodesPresenter whenSelectionChangedDo: [ :selection | self refreshWithLogSelection: selection ].
	self initializeRefreshOnSessionUpdate.
	self initializeWorkaroundToRefreshOnMonitorLogAnnouncement
]

{ #category : 'initialization' }
EpUnifiedBrowserPresenter >> initializePresenters [

	toolbarPresenter := self newToolbar.
	logNodesPresenter := self instantiate: EpLogNodeGraphPresenter.
	logPresenter := self instantiate: EpLogBrowserPresenter.

	logNodesPresenter whenSelectionChangedDo: [ :selection |
		self refreshWithLogSelection: selection ].

	self initializeToolbar.
	self initializeRefreshOnSessionUpdate.
	self initializeWorkaroundToRefreshOnMonitorLogAnnouncement
]

{ #category : 'initialization' }
EpUnifiedBrowserPresenter >> initializeRefreshOnSessionUpdate [
	self updatesAnnouncer weak when: OmSessionStoreUpdated send: #refresh to: self
]

{ #category : 'initialization' }
EpUnifiedBrowserPresenter >> initializeToolbar [

	toolbarPresenter
		add: (self newToolbarButton
			icon: (self application iconNamed: #refresh);
			label: 'Refresh';
			help: 'Refresh logs from this directory';
			action: [ self refreshLogs ];
			yourself);
		add: (self newToolbarButton
			icon: (self application iconNamed: #open);
			label: 'Directory';
			help: 'Select directory to browse logs (i.e. files with ombu extension)';
			action: [ self selectDirectory ];
			yourself);
		add: (self newToolbarButton
			icon: (self application iconNamed: #config);
			label: 'Dashboard';
			help: 'Open epicea dashboard';
			action: [ self openDashboard ];
			yourself)
]

{ #category : 'initialization' }
EpUnifiedBrowserPresenter >> initializeWindow: aWindowPresenter [

	super initializeWindow: aWindowPresenter.	
	aWindowPresenter
		whenClosedDo: [ self updatesAnnouncer unsubscribe: self ]
]

{ #category : 'initialization' }
EpUnifiedBrowserPresenter >> initializeWorkaroundToRefreshOnMonitorLogAnnouncement [
	refreshDeferrer := OmDeferrer send: #refreshIfMonitorLogSelected to: self after: 500 milliSeconds.

	self monitorLogAnnouncer weak when: EpEntryAdded send: #schedule to: refreshDeferrer.

	self whenWindowChanged: [ :windowModel |
		windowModel whenClosedDo: [
			self monitorLogAnnouncer unsubscribe: refreshDeferrer ] ]
]

{ #category : 'initialization' }
EpUnifiedBrowserPresenter >> monitorLogAnnouncer [
	^ EpMonitor current log announcer
]

{ #category : 'actions' }
EpUnifiedBrowserPresenter >> openDashboard [

	(EpDashboardPresenter newApplication: self application) open
]

{ #category : 'refreshing' }
EpUnifiedBrowserPresenter >> refresh [
	logNodesPresenter refresh
]

{ #category : 'refreshing' }
EpUnifiedBrowserPresenter >> refreshIfMonitorLogSelected [
	"See initializeWorkaroundToRefreshOnMonitorLogAnnouncement"

	(logNodesPresenter selectedLogNodes anySatisfy: [ :item | item isMonitor ])
		ifTrue: [ logPresenter refresh ]
]

{ #category : 'actions' }
EpUnifiedBrowserPresenter >> refreshLogs [

	logNodesPresenter refresh
]

{ #category : 'refreshing' }
EpUnifiedBrowserPresenter >> refreshWithLogSelection: selection [

	logPresenter
		theLog: (self freshLogFromSelection: selection);
		refresh;
		cleanEntryContent
]

{ #category : 'actions' }
EpUnifiedBrowserPresenter >> selectDirectory [

	logNodesPresenter selectDirectory
]

{ #category : 'initialization' }
EpUnifiedBrowserPresenter >> updatesAnnouncer [
	^ EpMonitor current sessionStore announcer
]

{ #category : 'initialization' }
EpUnifiedBrowserPresenter >> windowTitle [
	^  'Epicea - Code Changes'
		
]
