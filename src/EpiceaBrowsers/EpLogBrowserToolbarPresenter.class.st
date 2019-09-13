"
I am a toolbar to be used in the EpLogBrowserModel.
"
Class {
	#name : #EpLogBrowserToolbarPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'logBrowserPresenter',
		'filtersButton',
		'statusLabel'
	],
	#classInstVars : [
		'refresh'
	],
	#category : #'EpiceaBrowsers-UI-Log'
}

{ #category : #specs }
EpLogBrowserToolbarPresenter class >> defaultSpec [
	^ SpPanedLayout newHorizontal
		position: 40 percent;
		add: #statusLabel;
		add: #filtersButton;
		yourself
]

{ #category : #private }
EpLogBrowserToolbarPresenter >> activeFiltersString [
	^ 'Filters (' , logBrowserPresenter filters size asString , ')'
]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> beApplyMode [
	filtersButton
		icon: (self iconNamed: #smallRedoIcon);
		label: 'Confirm All Changes';
		action: [ logBrowserPresenter applyAllChangesAndCloseWindow ]
]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> beHistoryMode [
	filtersButton
		icon: (self iconNamed: #smallFindIcon);
		action: [ self filtersAction ]
]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> beRevertMode [
	filtersButton
		icon: (self iconNamed: #smallUndoIcon);
		label: 'Confirm All Changes';
		action: [ logBrowserPresenter revertAllChangesAndCloseWindow ]
]

{ #category : #private }
EpLogBrowserToolbarPresenter >> filtersAction [
	logBrowserPresenter filtersSubMenu openWithSpecAt: ActiveHand position
]

{ #category : #initialization }
EpLogBrowserToolbarPresenter >> initializeWidgets [
	filtersButton := self newButton.
	statusLabel := self newLabel
]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> logBrowserPresenter: anObject [
	logBrowserPresenter := anObject
]

{ #category : #refreshing }
EpLogBrowserToolbarPresenter >> refresh [
	filtersButton label: self activeFiltersString.
	statusLabel label: self statusString
]

{ #category : #refreshing }
EpLogBrowserToolbarPresenter >> statusString [
	^ String
		streamContents: [ :stream | 
			| filtered total |
			filtered := logBrowserPresenter filteredEntriesCount.
			total := logBrowserPresenter totalEntriesCount.

			stream
				print: filtered;
				nextPutAll: ' events'.

			filtered = total
				ifFalse: [ stream
						nextPutAll: ' (';
						print: total;
						nextPutAll: ' total)' ] ]
]
