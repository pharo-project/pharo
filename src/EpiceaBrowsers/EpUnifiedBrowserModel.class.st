"
I'm a Spec model for the main tool to browse Epicea logs.

Open with:
self open
"
Class {
	#name : #EpUnifiedBrowserModel,
	#superclass : #ComposableModel,
	#instVars : [
		'logModel',
		'logNodesModel',
		'monitorLog',
		'refreshDeferrer'
	],
	#category : #'EpiceaBrowsers-UI-Unified'
}

{ #category : #'instance creation' }
EpUnifiedBrowserModel class >> open [

	^ self new
		refresh;
		openWithSpec;
		yourself
]

{ #category : #accessing }
EpUnifiedBrowserModel class >> spec [
	<spec: #default>

	^ SpecLayout composed
		newRow: [ :row |
			row 
				add: #logNodesModel origin: 0@0 corner: 0.4@1;
				addSplitter;
				add: #logModel origin: 0.4@0 corner: 1@1 ]
]

{ #category : #accessing }
EpUnifiedBrowserModel class >> taskbarIconName [

	^ #recoverLostChangesIcon
]

{ #category : #specs }
EpUnifiedBrowserModel class >> title [

	^ 'Epicea - Code Changes'
]

{ #category : #accessing }
EpUnifiedBrowserModel class >> worldMenuItemOn: aBuilder [
	<worldMenu>

	(aBuilder item: 'Code Changes')
		parent: #Tools;
		action: [ self open ]; 
		icon: self taskbarIcon;
		help: 'Browse recorded change logs during from Pharo coding sessions and replay changes.';
		order: 0.5
]

{ #category : #api }
EpUnifiedBrowserModel >> initialExtent [ 
	^ 950 @ 650
]

{ #category : #initialization }
EpUnifiedBrowserModel >> initializePresenter [

	super initializePresenter.
	
	logNodesModel logNodesTreeModel
		whenSelectedItemChanged: [ :items | self refreshWithLogSelected: items ].
		
	self initializeRefreshOnSessionUpdate.
	self initializeWorkaroundToRefreshOnMonitorLogAnnouncement.
]

{ #category : #initialization }
EpUnifiedBrowserModel >> initializeRefreshOnSessionUpdate [

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
EpUnifiedBrowserModel >> initializeWidgets [

	self instantiateModels: {
		#logNodesModel -> #EpLogNodeGraphModel.
		#logModel -> #EpLogBrowserModel }
]

{ #category : #initialization }
EpUnifiedBrowserModel >> initializeWorkaroundToRefreshOnMonitorLogAnnouncement [

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
EpUnifiedBrowserModel >> logModel [
	^ logModel
]

{ #category : #accessing }
EpUnifiedBrowserModel >> logNodesModel [
	^ logNodesModel
]

{ #category : #refreshing }
EpUnifiedBrowserModel >> refresh [

	logNodesModel refresh.
]

{ #category : #refreshing }
EpUnifiedBrowserModel >> refreshIfMonitorLogSelected [
	"See initializeWorkaroundToRefreshOnMonitorLogAnnouncement"
	
	| item |
	item := logNodesModel logNodesTreeModel selectedItem.
	item ifNil: [ ^self ]. "Nothing to refresh"
	item content isMonitor ifFalse: [ ^self ]. "Monitor log isn't selected"
	logModel refresh.
]

{ #category : #refreshing }
EpUnifiedBrowserModel >> refreshWithLogSelected: item [

	| freshLog |
	freshLog := item
		ifNil: [ EpLog newNull ]
		ifNotNil: [ item log ].
	
	logModel
		theLog: freshLog;
		removeAllFilters;
		refresh;
		cleanEntryContentModel.

]
