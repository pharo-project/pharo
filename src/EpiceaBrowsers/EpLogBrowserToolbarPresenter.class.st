"
I am a toolbar to be used in the EpLogBrowserModel.
"
Class {
	#name : 'EpLogBrowserToolbarPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'logBrowserPresenter',
		'filtersButton',
		'statusLabel',
		'filteredEntriesCount',
		'searchedEntriesCount'
	],
	#category : 'EpiceaBrowsers-UI-Log',
	#package : 'EpiceaBrowsers',
	#tag : 'UI-Log'
}

{ #category : 'layout' }
EpLogBrowserToolbarPresenter class >> defaultLayout [
	^ SpBoxLayout newLeftToRight
		add: #statusLabel;
		addLast: #filtersButton;
		yourself
]

{ #category : 'private' }
EpLogBrowserToolbarPresenter >> activeFiltersString [
	^ 'Filters (' , logBrowserPresenter filters size asString , ')'
]

{ #category : 'accessing' }
EpLogBrowserToolbarPresenter >> beApplyMode [
	filtersButton
		icon: (self iconNamed: #smallRedo);
		label: 'Confirm All Changes';
		action: [ logBrowserPresenter applyAllChangesAndCloseWindow ]
]

{ #category : 'accessing' }
EpLogBrowserToolbarPresenter >> beHistoryMode [
	filtersButton
		icon: (self iconNamed: #smallFind);
		action: [
			logBrowserPresenter filtersSubMenu
				openWithSpecAt: self currentWorld activeHand position ]
]

{ #category : 'accessing' }
EpLogBrowserToolbarPresenter >> beRevertMode [
	filtersButton
		icon: (self iconNamed: #smallUndo);
		label: 'Confirm All Changes';
		action: [ logBrowserPresenter revertAllChangesAndCloseWindow ]
]

{ #category : 'accessing' }
EpLogBrowserToolbarPresenter >> filteredEntriesCount [
	^ filteredEntriesCount
]

{ #category : 'accessing' }
EpLogBrowserToolbarPresenter >> filteredEntriesCount: anInteger [ 

	filteredEntriesCount := anInteger
]

{ #category : 'initialization' }
EpLogBrowserToolbarPresenter >> initialize [ 

	super initialize.
	searchedEntriesCount := 0.
]

{ #category : 'initialization' }
EpLogBrowserToolbarPresenter >> initializePresenters [

	filtersButton := self newButton.
	statusLabel := self newLabel
]

{ #category : 'accessing' }
EpLogBrowserToolbarPresenter >> logBrowserPresenter: anObject [
	logBrowserPresenter := anObject
]

{ #category : 'refreshing' }
EpLogBrowserToolbarPresenter >> refresh [
	filtersButton label: self activeFiltersString.
	statusLabel label: self statusString
]

{ #category : 'accessing' }
EpLogBrowserToolbarPresenter >> searchedEntriesCount: anInteger [ 

	searchedEntriesCount := anInteger
]

{ #category : 'refreshing' }
EpLogBrowserToolbarPresenter >> statusString [
	^ String
		streamContents: [ :stream |
			| total |
			total := logBrowserPresenter totalEntriesCount.
			stream
				print: total;
				nextPutAll: ' events | ';
				print: searchedEntriesCount;
				nextPutAll: ' found | ';
				print: filteredEntriesCount;
				nextPutAll: ' filtered' ]
]
