"
I am a toolbar to be used in the EpLogBrowserModel.
"
Class {
	#name : #EpLogBrowserToolbarPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'filtersButtonPresenter',
		'logBrowserPresenter',
		'statusLabelPresenter'
	],
	#category : #'EpiceaBrowsers-UI-Log'
}

{ #category : #specs }
EpLogBrowserToolbarPresenter class >> spec [
	<spec: #default>

	^ SpecLayout composed newRow: [:row |
		row
			add: #statusLabelPresenter;
			add: #filtersButtonPresenter width: 150;
			yourself ].
]

{ #category : #private }
EpLogBrowserToolbarPresenter >> activeFiltersString [

	^ 'Filters (', self logBrowserPresenter filters size asString, ')'

]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> beApplyMode [

	self filtersButtonPresenter
		icon: (self iconNamed: #smallRedoIcon);
		label: 'Confirm All Changes';
		action: [ self logBrowserPresenter applyAllChangesAndCloseWindow ].

]

{ #category : #initialization }
EpLogBrowserToolbarPresenter >> beHistoryMode [

	self filtersButtonPresenter
		icon: (self iconNamed: #smallFindIcon);
		action: [ self filtersAction ]

]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> beRevertMode [

	self filtersButtonPresenter
		icon: (self iconNamed: #smallUndoIcon);
		label: 'Confirm All Changes';
		action: [ self logBrowserPresenter revertAllChangesAndCloseWindow ].

]

{ #category : #private }
EpLogBrowserToolbarPresenter >> filtersAction [

	self logBrowserPresenter filtersSubMenu openWithSpecAt: ActiveHand position

]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> filtersButtonPresenter [

	^ filtersButtonPresenter
]

{ #category : #initialization }
EpLogBrowserToolbarPresenter >> initializePresenter [
	"See be* methods"

]

{ #category : #initialization }
EpLogBrowserToolbarPresenter >> initializeWidgets [

	self instantiatePresenters: {
		#filtersButtonPresenter -> #ButtonPresenter.
		#statusLabelPresenter -> #LabelPresenter.
		}.

]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> logBrowserPresenter [
	^ logBrowserPresenter
]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> logBrowserPresenter: anObject [
	logBrowserPresenter := anObject
]

{ #category : #refreshing }
EpLogBrowserToolbarPresenter >> refresh [

	self filtersButtonPresenter label: self activeFiltersString.
	self statusLabelPresenter label: self statusString.
]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> statusLabelPresenter [

	^ statusLabelPresenter
]

{ #category : #refreshing }
EpLogBrowserToolbarPresenter >> statusString [

	^ String streamContents: [:stream |
		| filtered total |
		filtered := self logBrowserPresenter filteredEntriesCount.
		total := self logBrowserPresenter totalEntriesCount.
		
		stream
			print: filtered;
			nextPutAll: ' events'.
	
		filtered = total ifFalse: [
			stream
				nextPutAll: ' (';
				print: total;
				nextPutAll: ' total)' ].
		]
]
