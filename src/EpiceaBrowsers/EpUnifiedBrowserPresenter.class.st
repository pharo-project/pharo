"
I'm a Spec model for the main tool to browse Epicea logs.

Open with:
self open
"
Class {
	#name : #EpUnifiedBrowserPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'monitorLog',
		'refreshDeferrer',
		'logPresenter',
		'logNodesPresenter'
	],
	#category : #'EpiceaBrowsers-UI-Unified'
}

{ #category : #accessing }
EpUnifiedBrowserPresenter class >> defaultSpec [
	^ SpPanedLayout newHorizontal
		position: 40 percent;
		add: #logNodesPresenter;
		add: #logPresenter;
		yourself
]

{ #category : #'instance creation' }
EpUnifiedBrowserPresenter class >> open [

	^ self new
		refresh;
		openWithSpec;
		yourself
]

{ #category : #accessing }
EpUnifiedBrowserPresenter class >> taskbarIconName [

	^ #recoverLostChangesIcon
]

{ #category : #specs }
EpUnifiedBrowserPresenter class >> title [

	^ 'Epicea - Code Changes'
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

{ #category : #api }
EpUnifiedBrowserPresenter >> initialExtent [ 
	^ 950 @ 650
]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> initializePresenter [
	super initializePresenter.
	self logNodesPresenter logNodesTreePresenter whenSelectionChangedDo: [ :selection | self refreshWithLogSelected: selection selectedItem ].
	self initializeRefreshOnSessionUpdate.
	self initializeWorkaroundToRefreshOnMonitorLogAnnouncement
]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> initializeRefreshOnSessionUpdate [

	| updatesAnnouncer |
	updatesAnnouncer := EpMonitor current sessionStore announcer.

	updatesAnnouncer weak
		when: OmSessionStoreUpdated
		send: #refresh
		to: self.

	self whenWindowChanged: [ :windowModel | 
		windowModel whenClosedDo: [ 
			updatesAnnouncer unsubscribe: self ] ].
]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> initializeWidgets [
	logNodesPresenter := self instantiate: EpLogNodeGraphPresenter.
	logPresenter := self instantiate: EpLogBrowserPresenter
]

{ #category : #initialization }
EpUnifiedBrowserPresenter >> initializeWorkaroundToRefreshOnMonitorLogAnnouncement [

	monitorLog := EpMonitor current log.
	refreshDeferrer := OmDeferrer 
		send: #refreshIfMonitorLogSelected
		to: self
		after: 500 milliSeconds.

	monitorLog announcer weak
		when: EpEntryAdded
		send: #schedule
		to: refreshDeferrer.

	self whenWindowChanged: [ :windowModel | 
		windowModel whenClosedDo: [ 
			monitorLog announcer unsubscribe: refreshDeferrer ] ].
]

{ #category : #accessing }
EpUnifiedBrowserPresenter >> logNodesPresenter [
	^ logNodesPresenter
]

{ #category : #accessing }
EpUnifiedBrowserPresenter >> logPresenter [
	^ logPresenter
]

{ #category : #refreshing }
EpUnifiedBrowserPresenter >> refresh [

	self logNodesPresenter refresh.
]

{ #category : #refreshing }
EpUnifiedBrowserPresenter >> refreshIfMonitorLogSelected [
	"See initializeWorkaroundToRefreshOnMonitorLogAnnouncement"

	| item |
	item := self logNodesPresenter logNodesTreePresenter selectedItem.
	item ifNil: [ ^ self ].	"Nothing to refresh"
	item content isMonitor
		ifFalse: [ ^ self ].	"Monitor log isn't selected"
	self logPresenter refresh
]

{ #category : #refreshing }
EpUnifiedBrowserPresenter >> refreshWithLogSelected: item [

	| freshLog |
	freshLog := item
		ifNil: [ EpLog newNull ]
		ifNotNil: [ item log ].
	
	self logPresenter
		theLog: freshLog;
		removeAllFilters;
		refresh;
		cleanEntryContentModel.

]
