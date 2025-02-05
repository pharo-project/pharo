"
I am a Spec browser for an EpLog.

For example, open with:

self open

"
Class {
	#name : 'EpLogBrowserPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'log',
		'andFilter',
		'selectionDeferrer',
		'cachedLogEntries',
		'commentsLog',
		'cachedItems',
		'showEntryItemMenu',
		'entryContentPresenter',
		'itemsPresenter',
		'toolbarPresenter',
		'searchPresenter',
		'searchButtonPresenter'
	],
	#category : 'EpiceaBrowsers-UI-Log',
	#package : 'EpiceaBrowsers',
	#tag : 'UI-Log'
}

{ #category : 'layout' }
EpLogBrowserPresenter class >> defaultLayout [

	^ SpBoxLayout newTopToBottom 
			add: (SpBoxLayout newLeftToRight 
				add: #searchPresenter expand: true fill: true padding: 3;
				add: #searchButtonPresenter expand: false;
				yourself) expand: false fill: false padding: 3;
			add: (SpPanedLayout newTopToBottom
					positionOfSlider: 0.6;
					add: #itemsPresenter;
					add: (SpBoxLayout newTopToBottom
					   add: #toolbarPresenter
						   expand: false
						   fill: false
						   padding: 2;
					   add: #entryContentPresenter
						   expand: true
						   fill: true
						   padding: 2;
						   yourself);
			yourself);
			yourself
]

{ #category : 'accessing' }
EpLogBrowserPresenter class >> defaultLog [

	^ EpMonitor current log
]

{ #category : 'accessing' }
EpLogBrowserPresenter class >> defaultPreferredExtent [

	^ 550@700
]

{ #category : 'instance creation' }
EpLogBrowserPresenter class >> newWithLog: aLog [

	^ self new
		theLog: aLog;
		yourself
]

{ #category : 'instance creation' }
EpLogBrowserPresenter class >> open [
	<script>
	^ self openWithLog: self defaultLog
]

{ #category : 'instance creation' }
EpLogBrowserPresenter class >> openWithLog: aLog [

	^ (self newWithLog: aLog)
		  refresh;
		  open;
		  yourself
]

{ #category : 'private' }
EpLogBrowserPresenter >> addFilter: aFilter [

	(andFilter subFilters includes: aFilter) ifTrue: [ ^self ].

	andFilter := EpAndFilter withAll: (andFilter subFilters copyWith: aFilter).
	self refresh
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> applyAllChangesAndCloseWindow [

	self applyChangesIn: self entriesWithCodeChanges.
	self window close
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> applyChangesIn: entries [

	entries ifEmpty: [ ^ self inform: 'No code changes to apply' ].

	EpLogBrowserOperationFactory new
		logBrowserModel: self;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		applyCodeChanges
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> applyCompleteRefactoringInSelection [

	| entries |
	entries := self selectedEntries.
	entries size = 1 ifFalse: [
		^ self inform: 'Please select one refactoring' ]. "Only implemented for one"

	EpLogBrowserOperationFactory new
		logBrowserModel: self;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		applyCompleteRefactoring
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> applyPropagateRefactoringInSelection [

	| entries |
	entries := self selectedEntries.
	entries size = 1 ifFalse: [
		^ self inform: 'Please select one refactoring' ]. "Only implemented for one"

	EpLogBrowserOperationFactory new
		logBrowserModel: self;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		applyPropagateRefactoring
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> applySelectedChanges [

	self applyChangesIn: self selectedCodeChanges
]

{ #category : 'modes' }
EpLogBrowserPresenter >> beApplyMode [
	self withWindowDo: [ :window | window title: 'Epicea - Apply Changes' ].
	toolbarPresenter beApplyMode.
	showEntryItemMenu := false
]

{ #category : 'modes' }
EpLogBrowserPresenter >> beHistoryMode [
	self withWindowDo: [ :window | window title: 'Epicea - Log Browser' ].
	toolbarPresenter beHistoryMode.
	showEntryItemMenu := true
]

{ #category : 'modes' }
EpLogBrowserPresenter >> beRevertMode [
	self withWindowDo: [ :window | window title: 'Epicea - Revert Changes' ].
	toolbarPresenter beRevertMode.
	showEntryItemMenu := false
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> browseSelection [

	| selectionSize |
	selectionSize := self selectedEntryReferences size.
	selectionSize > 3 ifTrue: [
		(self confirm: 'Do you want to open a browser for each code changes? (', selectionSize asString, ' items selected)')
			ifFalse: [ ^self ] ].

	self selectedEntryItems
		select: [ :each | each entry content isCodeChange ]
		thenDo: [ :each | each eventAccept: (EpBrowseVisitor new application: self application; yourself) ]
]

{ #category : 'private' }
EpLogBrowserPresenter >> cachedItems [

	^ cachedItems ifNil: [ cachedItems := WeakValueDictionary new ]
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> cachedLogEntries [
	"Answer log entries.
	Check size since the log might be current Monitor's, so it can grow."

	^ (cachedLogEntries isNotNil and: [ cachedLogEntries size = log entriesCount ])
		ifTrue: [ cachedLogEntries ]
		ifFalse: [
			[ : job | 
				job title: 'Reading log'.
				cachedLogEntries := log entries
			] asJob run ].

]

{ #category : 'refreshing' }
EpLogBrowserPresenter >> cleanEntryContent [
	self refreshEntryContentWith: #()
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> commentsLog [

	^ commentsLog ifNil: [ self monitor log ]
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> commentsLog: anEpLog [

	commentsLog := anEpLog
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> entries [

	^ log entryReferences collect: [:each | log entryFor: each ]
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> entriesWithCodeChanges [

	^ self entries select: [:each | each content isCodeChange ]
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> entryItems [

	^ log entryReferences collect: [:each |
		self itemForEntryReference: each ]
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> fileOutSelection [
	"Write the selected entries to a file"

	| entries |
	entries := self selectedCodeChanges collect: [ :each | each content ].

	EpChunkExporter fileOut: entries.

]

{ #category : 'menu' }
EpLogBrowserPresenter >> filterActions [

	^ SpActionGroup new
		beDisplayedAsSubMenu;
		name: 'Filters';
		addActionWith: [ :anItem | anItem
			name: 'Clean action filters';
			iconName: #smallDelete;
			action: [ self removeAllFilters ];
			actionEnabled: [ self filters isEmptyOrNil not ] ];
		addGroupWith: [ :aGroup | aGroup 
			beDisplayedAsGroup;
			addActionWith: [ :anItem | anItem
				name: EpLatestCodeChangeFilter title;
				description: EpLatestCodeChangeFilter comment;
				iconName: #smallFind;
				action: [ self filterLatestCodeChanges ] ];
			 addActionWith: [ :anItem | anItem
				name: EpImpactCodeChangeFilter title;
				description: EpImpactCodeChangeFilter comment;
				iconName: #smallFind;
				action: [ self filterImpactCodeChanges ] ] ];
		addGroupWith: [ :aGroup | aGroup
			beDisplayedAsGroup; 
			addActionWith: [ :anItem | anItem
				name: EpEntryReferencesFilter title;
				iconName: #smallFind;
				action: [ self filterSelection ] ];
			addActionWith: [ :anItem | anItem
				name: 'Show only changes to this method';
				description: 'Show only additions, modifications or removals of this method.';
				iconName: #smallFind;
				action: [ self filterAllChangesToSelection ] ] ;
			addActionWith: [ :anItem | anItem
				name: 'Show only changes to this class or trait';
				description: 'Show only changes in the scope of this class or trait.';
				iconName: #smallFind;
				action: [ self filterAllChangesToSelectionBehavior ] ];
			 addActionWith: [ :anItem | anItem
				name: 'Show only changes in this package';
				description: 'Show only additions, modifications or removals of this package.';
				iconName: #smallFind;
				action: [ self filterAllChangesToSelectionPackage ] ] ];
		addGroupWith: [ :aGroup | aGroup
			beDisplayedAsGroup; 
			addActionWith: [ :anItem | anItem
				name: 'Show only if done today';
				iconName: #calendar;
				action: [ self filterToday ] ];
			addActionWith: [ :anItem | anItem
				name: 'Hide events done after';
				iconName: #calendar;
				action: [ self filterBefore ] ]; 
			addActionWith: [ :anItem | anItem
				name: 'Hide events done before';
				iconName: #calendar;
				action: [ self filterAfter ] ] ];
		addGroupWith: [ :aGroup | 
			aGroup beDisplayedAsGroup.
			EpPluggableFilter allFilters do: [ :aFilter |
				aGroup addActionWith: [ :anItem | anItem
					name: aFilter title;
					iconName: #smallFind;
					description: aFilter title;
					action: [ self addFilter: aFilter ] ] ] ];
		yourself

]

{ #category : 'menu - filters' }
EpLogBrowserPresenter >> filterAfter [

	self selectedEntryItems ifNotEmpty: [ :items |
		| time |
		time := items first entry tags at: EpLog timeKey.
		self addFilter: (EpPluggableFilter after: time) ]
]

{ #category : 'menu - filters' }
EpLogBrowserPresenter >> filterAllChangesToSelection [

	| changes |
	changes := (self selectedEntryItems collect: [ :each | each entry content ]).

	changes size = 1 ifFalse: [ ^ self inform: 'Should select a single method change' ].
	changes anyOne isEpMethodChange ifFalse: [ ^ self inform: 'Should select a method' ].

	self addFilter: (EpPluggableFilter isMethod: changes anyOne methodAffected)
]

{ #category : 'menu - filters' }
EpLogBrowserPresenter >> filterAllChangesToSelectionBehavior [

	| changes |
	changes := self selectedEntryItems collect: [ :each | each entry content ].

	changes size = 1 ifFalse: [ ^ self inform: 'Should select a single change, either to a class or a trait' ].
	changes anyOne isEpMethodChange ifFalse: [ ^ self inform: 'Should select a method' ].

	self addFilter: (EpPluggableFilter isBehaviorNamed: changes anyOne behaviorAffectedName)
]

{ #category : 'menu - filters' }
EpLogBrowserPresenter >> filterAllChangesToSelectionPackage [

	| changes |
	changes := (self selectedEntryItems collect: [ :each | each entry content ]).

	changes size = 1 ifFalse: [ ^ self inform: 'Should select a single change' ].

	self addFilter: (EpPluggableFilter isPackageNamed: changes anyOne affectedPackageName)
]

{ #category : 'menu - filters' }
EpLogBrowserPresenter >> filterBefore [

	self selectedEntryItems ifNotEmpty: [ :items |
		| time |
		time := items last entry tags at: EpLog timeKey.
		self addFilter: (EpPluggableFilter before: time) ]
]

{ #category : 'menu - filters' }
EpLogBrowserPresenter >> filterImpactCodeChanges [

	self addFilter: (EpImpactCodeChangeFilter new environment: self class environment; yourself)
]

{ #category : 'menu - filters' }
EpLogBrowserPresenter >> filterLatestCodeChanges [

	self addFilter: (EpLatestCodeChangeFilter new logBrowserModel: self; yourself)
]

{ #category : 'menu - filters' }
EpLogBrowserPresenter >> filterSelection [

	| references |
	references := self selectedEntryReferences.

	references ifEmpty: [ ^self inform: 'Selection is empty' ].

	self addFilter: (EpEntryReferencesFilter new
		theLog: log;
		rejectedEntryReferences: references;
		yourself)
]

{ #category : 'menu - filters' }
EpLogBrowserPresenter >> filterToday [

	self addFilter: EpPluggableFilter todayFilter
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> filteredEntries [

	^ self filteredEntryReferences collect: [:each | log entryFor: each ]
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> filteredEntriesCount [

	^ self filteredEntryReferences size
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> filteredEntryReferences [

	| filteredEntries |
	filteredEntries := andFilter subFilters
		ifEmpty: [ log entryReferences ]
		ifNotEmpty: [
			self cachedLogEntries
				select: [:each | andFilter accepts: each ]
				thenCollect: [:each | log referenceTo: each ] ].
	toolbarPresenter filteredEntriesCount: filteredEntries size.
	^ filteredEntries
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> filters [

	^ andFilter subFilters
]

{ #category : 'menu' }
EpLogBrowserPresenter >> filtersSubMenu [

	| aMenu |
	aMenu := SpMenuPresenter new.

	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem
				name: 'Clean filters (', self filters size asString, ' active)';
				icon: (self iconNamed: #smallDelete);
				action: [ self removeAllFilters ] ].
		].

	aMenu addGroup: [ :aGroup |

		aGroup addItem: [ :anItem |
			anItem
				name: EpLatestCodeChangeFilter title;
				description: EpLatestCodeChangeFilter comment;
				icon: (self iconNamed: #smallFind);
				action: [ self filterLatestCodeChanges ] ].

		aGroup addItem: [ :anItem |
			anItem
				name: EpImpactCodeChangeFilter title;
				description: EpImpactCodeChangeFilter comment;
				icon: (self iconNamed: #smallFind);
				action: [ self filterImpactCodeChanges ] ].
	].

	aMenu addGroup: [ :aGroup |

		aGroup addItem: [ :anItem |
			anItem
				name: EpEntryReferencesFilter title;
				icon: (self iconNamed: #smallFind);
				action: [ self filterSelection ]
			].

		aGroup addItem: [ :anItem |
			anItem
				name: 'Show only changes to this method';
				description: 'Show only additions, modifications or removals of this method.';
				icon: (self iconNamed: #smallFind);
				action: [ self filterAllChangesToSelection ] ].

		aGroup addItem: [ :anItem |
			anItem
				name: 'Show only changes to this class or trait';
				description: 'Show only changes in the scope of this class or trait.';
				icon: (self iconNamed: #smallFind);
				action: [ self filterAllChangesToSelectionBehavior ]
			].

		aGroup addItem: [ :anItem |
			anItem
				name: 'Show only changes in this package';
				description: 'Show only additions, modifications or removals of this package.';
				icon: (self iconNamed: #smallFind);
				action: [ self filterAllChangesToSelectionPackage ]
			].
	].


	aMenu addGroup: [ :aGroup |

		aGroup addItem: [ :anItem |
			anItem
				name: 'Show only if done today';
				icon: (self iconNamed: #calendar);
				action: [ self filterToday ] ].

		aGroup addItem: [ :anItem |
			anItem
				name: 'Hide events done after';
				icon: (self iconNamed: #calendar);
				action: [ self filterBefore ] ].

		aGroup addItem: [ :anItem |
			anItem
				name: 'Hide events done before';
				icon: (self iconNamed: #calendar);
				action: [ self filterAfter ] ].
	].

	aMenu addGroup: [ :aGroup |
		EpPluggableFilter allFilters do: [ :aFilter |
			aGroup addItem: [ :anItem |
				anItem
					name: aFilter title;
					icon: (self iconNamed: #smallFind);
					description: aFilter title;
					action: [ self addFilter: aFilter ] ] ]
		].

	^ aMenu
]

{ #category : 'initialization' }
EpLogBrowserPresenter >> initialize [
	super initialize.

	andFilter := EpAndFilter withAll: #()
]

{ #category : 'initialization' }
EpLogBrowserPresenter >> initializePresenters [

	itemsPresenter := self newList.
	itemsPresenter
		beMultipleSelection;
		display: [ :entryReference | self morphForItemAt: entryReference ];
		whenSelectionChangedDo: [ self refreshEntryContentDeferrer schedule ];
		actions: self menuActions.
		
	self initializeSearchPresenters.

	toolbarPresenter := self instantiate: EpLogBrowserToolbarPresenter.
	toolbarPresenter logBrowserPresenter: self.

	entryContentPresenter := self newDiff.
	entryContentPresenter showOptions: false.

	self beHistoryMode
]

{ #category : 'initialization' }
EpLogBrowserPresenter >> initializeSearchPresenters [

	searchPresenter := self newTextInput
		placeholder: 'Filter...';
		whenTextChangedDo: [ : text | 
			text 
				ifNotEmpty: [ searchButtonPresenter enable ]
				ifEmpty: [ searchButtonPresenter disable ].
			self searchList: text ];
		yourself.
	searchButtonPresenter := self newButton
		iconName: #smallFind;
		label: 'Find';
		disable;
		action: [ self searchList: searchPresenter text ];
		yourself.
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> inspectSelection [

	self selectedEntryItems do: #inspect
]

{ #category : 'private' }
EpLogBrowserPresenter >> itemForEntryReference: entryReference [

	^ self cachedItems
		at: entryReference
		ifPresent: [:item | item ]
		ifAbsentOrNil: [
			| newItem |
			newItem := self newItemForEntryReference: entryReference.
			self cachedItems at: entryReference put: newItem.
			newItem ]
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> itemsPresenter [
	^ itemsPresenter
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> log [
	^ log
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> logOffEvent: anEvent [

	self commentsLog addEntryWith: anEvent tags: [ :tags |  ]
]

{ #category : 'menu' }
EpLogBrowserPresenter >> menuActions [

	^ SpActionGroup new
		addActionWith: [ :action | action 
			name: 'Apply';
			description: 'Preview the application of selected code changes (open a new window to show the effect of re-applying the changes in current image)';
			iconName: #smallRedo;
			shortcutKey: $p actionModifier;
			action: [ self openPreviewToApplySelectedChanges ];
			actionEnabled: [ self itemsPresenter selectedItem isNotNil ] ];
		addActionWith: [ :action | action 
			name: 'Revert';
			description: 'Preview the reversion of selected code changes (open a new window to show the effect of reverting the changes in current image)';
			iconName: #smallUndo;
			shortcutKey: $r actionModifier;
			action: [ self openPreviewToRevertSelectedChanges ];
			actionEnabled: [ self itemsPresenter selectedItem isNotNil ] ];
		addActionWith: [ :action | action 
			name: 'Browse current';
			description: 'Browse working copy of entity affected in the selected event(s)';
			iconName: #nautilusIcon;
			shortcutKey: $b actionModifier;
			action: [ self browseSelection ];
			actionEnabled: [ self itemsPresenter selectedItem isNotNil ] ];
		addActionWith: [ :action | action 
			name: 'File out';
			description: 'File out the selected code changes';
			iconName: #smallSave;
			shortcutKey: $s actionModifier;
			action: [ self fileOutSelection ];
			actionEnabled: [ self itemsPresenter selectedItem isNotNil ] ];
		add: self filterActions;
		yourself
		
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> monitor [

	^ EpMonitor current
]

{ #category : 'private' }
EpLogBrowserPresenter >> morphForItemAt: entryReference [

	^ [ (self itemForEntryReference: entryReference) asMorph ]
			on: NotFound do: [:error | error asString asMorph ] "Enforce... remove it?"
]

{ #category : 'private' }
EpLogBrowserPresenter >> newItemForEntryReference: entryReference [

	| entry triggers|
	entry := log entryFor: entryReference.

	triggers := [ self triggerItemsFor: entry ]
		on: NotFound
		do: [ #() ]. "Just hide it... not an esential information"

	^ EpEntryItem new
		browser: self;
		entry: entry;
		application: self application;
		triggerItems: triggers;
		yourself
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> openPreviewToApplySelectedChanges [

	| entries previewLog |
	entries := self selectedCodeChanges.
	entries ifEmpty: [ ^ self inform: 'No changes to preview apply' ].

	previewLog := EpLogBrowserOperationFactory new
		logBrowserModel: self;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		newApplyPreviewLog.

	previewLog isEmpty
		ifTrue: [ self inform: 'Preview is empty (no changes to apply in current image)' ]
		ifFalse: [ previewLog browseEvents beApplyMode ]
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> openPreviewToRevertSelectedChanges [

	| entries previewLog |
	entries := self selectedCodeChanges.
	entries ifEmpty: [ ^ self inform: 'No changes to preview revert' ].

	previewLog := EpLogBrowserOperationFactory new
		logBrowserModel: self;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		newRevertPreviewLog.

	previewLog isEmpty
		ifTrue: [ self inform: 'Preview is empty (no changes to revert in current image)' ]
		ifFalse: [ previewLog browseEvents beRevertMode ]
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> operationsErrorHandlerBlock [

	^ [ :error | | message |
		message := self title, ' error on operation: ', error asString.
		message traceCr.
		self inform: message. ]
]

{ #category : 'menu' }
EpLogBrowserPresenter >> refactoringMenuActions [

	^ {
		{ 'Apply refactoring'.
		#applyCompleteRefactoringInSelection.
		'Apply selected refactoring'.
		#smallRedo.
		$y command }.
		{ 'Apply secondary effects of refactoring'.
		#applyPropagateRefactoringInSelection.
		'Apply only the secondary effects of the selected refactoring'.
		#smallRedo.
		$s command }
	}
]

{ #category : 'refreshing' }
EpLogBrowserPresenter >> refresh [
	"Refresh log"

	itemsPresenter items: self filteredEntryReferences reversed.
	toolbarPresenter refresh
]

{ #category : 'refreshing' }
EpLogBrowserPresenter >> refreshEntryContent [
	"Refresh the change list. I defer this action, so I'm prepated to be called from a non-UI process, such as the one of `OmDeferrer`. See: https://github.com/pharo-project/pharo/issues/9638"

	self defer: [ self refreshEntryContentWith: self selectedEntryItems ]
]

{ #category : 'refreshing' }
EpLogBrowserPresenter >> refreshEntryContentDeferrer [

	^ selectionDeferrer ifNil: [
		selectionDeferrer := OmDeferrer
			send: #refreshEntryContent
			to: self
			after: 150 milliSeconds ]
]

{ #category : 'refreshing' }
EpLogBrowserPresenter >> refreshEntryContentWith: selectedItems [
	selectedItems size = 1
		ifTrue: [ | item |
			item := selectedItems anyOne.
			entryContentPresenter
				showOnlyDestination: item oldStateString isEmptyOrNil;
				showOnlySource: item newStateString isEmptyOrNil;
				leftText: item oldStateString;
				rightText: item newStateString ]
		ifFalse: [ entryContentPresenter
				leftText: '';
				rightText: '';
				showOnlyDestination: true ]
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> removeAllFilters [

	andFilter := EpAndFilter withAll: #().
	self refresh
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> removeFilter: aFilter [

	andFilter := EpAndFilter withAll: (andFilter subFilters copyWithout: aFilter).
	self refresh
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> revertAllChangesAndCloseWindow [

	self revertChangesIn: self entriesWithCodeChanges.
	self window close
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> revertChangesIn: entries [

	entries ifEmpty: [ ^ self inform: 'No code changes to revert' ].

	EpLogBrowserOperationFactory new
		logBrowserModel: self;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		revertCodeChangesInPreviewMode
]

{ #category : 'menu - operations' }
EpLogBrowserPresenter >> revertSelectedChanges [

	self revertChangesIn: self selectedCodeChanges
]

{ #category : 'accessing - searching' }
EpLogBrowserPresenter >> searchEntries: entriesCollection for: aString [
	"Answer a <Collection> where each entry from entriesCollection matches aString"

	^ entriesCollection
		select: [ : omEntry | omEntry matches: aString ]
		thenCollect: [:each | log referenceTo: each ]
]

{ #category : 'accessing - searching' }
EpLogBrowserPresenter >> searchList: aString [
	"Update the receiver's log entries with entries matching aString"

	itemsPresenter items: (self searchedEntryMatches: aString) reversed.
	toolbarPresenter refresh.
]

{ #category : 'accessing - searching' }
EpLogBrowserPresenter >> searchedEntryMatches: aString [
	"If there are filters applied, then search aString in the subset of the filtered entries, otherwise search in the cached log entries"

	| searchedEntries |
	
	aString 
		ifNotEmpty: [ 
			searchedEntries := andFilter subFilters
				ifEmpty: [ self searchEntries: self cachedLogEntries for: aString ]
				ifNotEmpty: [ self searchEntries: self filteredEntries for: aString ].
			toolbarPresenter searchedEntriesCount: searchedEntries size.
			^ searchedEntries ]
		ifEmpty: [ toolbarPresenter searchedEntriesCount: 0 ].
	^ self filteredEntryReferences.


]

{ #category : 'menu - accessing' }
EpLogBrowserPresenter >> selectedCodeChanges [

	^ self selectedEntryItems
		select: [ :each | each entry content isCodeChange ]
		thenCollect: [ :each | each entry ]
]

{ #category : 'menu - accessing' }
EpLogBrowserPresenter >> selectedEntries [

	^ self selectedEntryItems collect: [:each | each entry]
]

{ #category : 'menu - accessing' }
EpLogBrowserPresenter >> selectedEntryItems [

	^ self selectedEntryReferences collect: [:each |
		self itemForEntryReference: each ]
]

{ #category : 'menu - accessing' }
EpLogBrowserPresenter >> selectedEntryReferences [
	"Before there was a workaround here to reject nils because it used a FastTablePresenter instead of a list presenter. I have the impression that the workaround is not needed with ListPresenter and I removed the workaround. In case you find a bug were we get nils here, just report it and we will add back the workaround to reject nils with a better comment."

	^ self itemsPresenter selectedItemsSorted reversed
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> theLog [
	^ log
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> theLog: aLog [

	cachedItems := nil.
	cachedLogEntries := nil.
	log := aLog
]

{ #category : 'accessing' }
EpLogBrowserPresenter >> totalEntriesCount [

	^ self theLog entriesCount
]

{ #category : 'private' }
EpLogBrowserPresenter >> triggerItemsFor: anEntry [

	^ self log
		triggererReferenceOf: anEntry
		ifPresent: [ :triggerEntryReference |
			| triggerItem |
			triggerItem := self itemForEntryReference: triggerEntryReference.
			triggerItem triggerItems, { triggerItem } ]
		ifAbsent: [ #() ]
]
