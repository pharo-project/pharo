"
I'm a Spec model for the main tool to browse Epicea logs.

Open with:
self open
"
Class {
	#name : #EpUnifiedBrowserPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'refreshDeferrer',
		'logPresenter',
		'logNodesPresenter'
	],
	#category : #'EpiceaBrowsers-UI-Unified'
}

{ #category : #specs }
EpUnifiedBrowserPresenter class >> defaultSpec [
	^ SpPanedLayout newHorizontal
		position: 40 percent;
		add: #logNodesPresenter;
		add: #logPresenter;
		yourself
]

{ #category : #'instance creation' }
EpUnifiedBrowserPresenter class >> open [
	<script>
	^ self new
		refresh;
		openWithSpec;
		yourself
]

{ #category : #accessing }
EpUnifiedBrowserPresenter class >> taskbarIconName [

	^ #recoverLostChangesIcon
]

{ #category : #accessing }
EpUnifiedBrowserPresenter class >> worldMenuItemOn: aBuilder [
	<worldMenu>

	(aBuilder item: 'Code Changes')
		parent: #Tools;
		action: [ self open ]; 
		icon: self taskbarIcon;
		help: 'Browse recorded change logs during from Pharo coding sessions and replay changes.';
		order: 401
]

{ #category : #refreshing }
EpUnifiedBrowserPresenter >> freshLogFromSelection: selection [
	^ selection selectedItems
		ifEmpty: [ EpLog newNull ]
		ifNotEmpty: [ :items | 
			| entries |
			"Needed to reverse the selected logs to have the expected order (as user)."
			entries := items reversed flatCollect: [:each | each log entries ].
			EpLog newWithEntries: entries ].


]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> initializePresenter [
	super initializePresenter.
	logNodesPresenter whenSelectionChangedDo: [ :selection | self refreshWithLogSelection: selection ].
	self initializeRefreshOnSessionUpdate.
	self initializeWorkaroundToRefreshOnMonitorLogAnnouncement
]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> initializePresenters [
	logPresenter := self instantiate: EpLogBrowserPresenter.

	logNodesPresenter := self instantiate: EpLogNodeGraphPresenter.
	logNodesPresenter whenSelectionChangedDo: [ :selection |
		self refreshWithLogSelection: selection ].

	self initializeRefreshOnSessionUpdate.
	self initializeWorkaroundToRefreshOnMonitorLogAnnouncement
]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> initializeRefreshOnSessionUpdate [
	self updatesAnnouncer weak when: OmSessionStoreUpdated send: #refresh to: self
]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: 'Epicea - Code Changes';
		initialExtent: 950 @ 650;
		whenClosedDo: [ self updatesAnnouncer unsubscribe: self ]
]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> initializeWorkaroundToRefreshOnMonitorLogAnnouncement [
	refreshDeferrer := OmDeferrer send: #refreshIfMonitorLogSelected to: self after: 500 milliSeconds.

	self monitorLogAnnouncer weak when: EpEntryAdded send: #schedule to: refreshDeferrer.

	self whenWindowChanged: [ :windowModel |
		windowModel whenClosedDo: [
			self monitorLogAnnouncer unsubscribe: refreshDeferrer ] ]
]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> monitorLogAnnouncer [
	^ EpMonitor current log announcer
]

{ #category : #refreshing }
EpUnifiedBrowserPresenter >> refresh [
	logNodesPresenter refresh
]

{ #category : #refreshing }
EpUnifiedBrowserPresenter >> refreshIfMonitorLogSelected [
	"See initializeWorkaroundToRefreshOnMonitorLogAnnouncement"

	(logNodesPresenter selectedLogNodes anySatisfy: [ :item | item isMonitor ])
		ifTrue: [ logPresenter refresh ]
]

{ #category : #refreshing }
EpUnifiedBrowserPresenter >> refreshWithLogSelection: selection [

	logPresenter
		theLog: (self freshLogFromSelection: selection);
		refresh;
		cleanEntryContent
]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> updatesAnnouncer [
	^ EpMonitor current sessionStore announcer
]
