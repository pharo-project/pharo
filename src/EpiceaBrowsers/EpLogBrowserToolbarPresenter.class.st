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
	#category : #'EpiceaBrowsers-UI-Log'
}

{ #category : #specs }
EpLogBrowserToolbarPresenter class >> defaultSpec [
	^ SpBoxLayout newHorizontal
		add: #statusLabel;
		addLast: #filtersButton;
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
		action: [
			logBrowserPresenter filtersSubMenu
				openWithSpecAt: ActiveHand position ]
]

{ #category : #accessing }
EpLogBrowserToolbarPresenter >> beRevertMode [
	filtersButton
		icon: (self iconNamed: #smallUndoIcon);
		label: 'Confirm All Changes';
		action: [ logBrowserPresenter revertAllChangesAndCloseWindow ]
]

{ #category : #initialization }
EpLogBrowserToolbarPresenter >> initializePresenters [
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
