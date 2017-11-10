"
I am a toolbar to be used in the EpLogBrowserModel.
"
Class {
	#name : #EpLogBrowserToolbarModel,
	#superclass : #ComposableModel,
	#instVars : [
		'filtersButtonModel',
		'logBrowserModel',
		'statusLabelModel'
	],
	#category : #EpiceaBrowsers-UI-Log
}

{ #category : #specs }
EpLogBrowserToolbarModel class >> spec [
	<spec: #default>

	^ SpecLayout composed newRow: [:row |
		row
			add: #statusLabelModel;
			add: #filtersButtonModel width: 150;
			yourself ].
]

{ #category : #private }
EpLogBrowserToolbarModel >> activeFiltersString [

	^ 'Filters (', logBrowserModel filters size asString, ')'

]

{ #category : #accessing }
EpLogBrowserToolbarModel >> beApplyMode [

	filtersButtonModel
		icon: (self iconNamed: #smallRedoIcon);
		label: 'Confirm All Changes';
		action: [ logBrowserModel applyAllChangesAndCloseWindow ].

]

{ #category : #initialization }
EpLogBrowserToolbarModel >> beHistoryMode [

	filtersButtonModel
		icon: (self iconNamed: #smallFindIcon);
		action: [ self filtersAction ]

]

{ #category : #accessing }
EpLogBrowserToolbarModel >> beRevertMode [

	filtersButtonModel
		icon: (self iconNamed: #smallUndoIcon);
		label: 'Confirm All Changes';
		action: [ logBrowserModel revertAllChangesAndCloseWindow ].

]

{ #category : #private }
EpLogBrowserToolbarModel >> filtersAction [

	logBrowserModel filtersSubMenu openWithSpecAt: ActiveHand position

]

{ #category : #accessing }
EpLogBrowserToolbarModel >> filtersButtonModel [

	^ filtersButtonModel
]

{ #category : #initialization }
EpLogBrowserToolbarModel >> initializePresenter [
	"See be* methods"

]

{ #category : #initialization }
EpLogBrowserToolbarModel >> initializeWidgets [

	self instantiateModels: {
		#filtersButtonModel -> #ButtonModel.
		#statusLabelModel -> #LabelModel.
		}.

]

{ #category : #accessing }
EpLogBrowserToolbarModel >> logBrowserModel [
	^ logBrowserModel
]

{ #category : #accessing }
EpLogBrowserToolbarModel >> logBrowserModel: anObject [
	logBrowserModel := anObject
]

{ #category : #refreshing }
EpLogBrowserToolbarModel >> refresh [

	filtersButtonModel label: self activeFiltersString.
	statusLabelModel label: self statusString.
]

{ #category : #accessing }
EpLogBrowserToolbarModel >> statusLabelModel [

	^ statusLabelModel
]

{ #category : #refreshing }
EpLogBrowserToolbarModel >> statusString [

	^ String streamContents: [:stream |
		| filtered total |
		filtered := logBrowserModel filteredEntriesCount.
		total := logBrowserModel totalEntriesCount.
		
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
