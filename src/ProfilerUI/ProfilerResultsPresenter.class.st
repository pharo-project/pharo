"
I am a panel in the ProfilerPresenter containing the profiling process results together with parameters to filter these results
"
Class {
	#name : #ProfilerResultsPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'treeTable',
		'viewModel',
		'codePresenter',
		'browseButton',
		'treeDropList',
		'expandAllButton',
		'collapseAllButton',
		'thresholdFilterInput',
		'thresholdExpandInput',
		'navigationToolbar',
		'reportButton',
		'showAsRootButton'
	],
	#category : #'ProfilerUI-View'
}

{ #category : #private }
ProfilerResultsPresenter >> goInto: aNode [

	| toolbarButtons clickedButton |
	toolbarButtons := navigationToolbar items select: [ :item | 
		                  item object == aNode ].

	toolbarButtons
		ifEmpty: [ "Either the button in the navigationBar must be created 
			(ie, 'see as root' in the context menu was clicked)"
			self removeToolbarItemsAfter: navigationToolbar selectedItem.
			clickedButton := self newToolbarButtonFrom: aNode.
			navigationToolbar addItem: clickedButton ]
		ifNotEmpty: [ "or it already exists (ie, the toolbar button was clicked)"
			clickedButton := toolbarButtons anyOne.
			clickedButton == navigationToolbar selectedItem ifTrue: [ ^ self ] ].

	self updateResults.
	self selectToolbarItem: clickedButton.
	
	toolbarButtons
		ifEmpty: [ 
			treeTable expandAllSuchThat: [ :item | 
				item weightToRoot asFloat closeTo: 100.0 ].
			treeTable adapter widget allNodeMorphs ifNotEmpty: [ :nodes |
				"select node displayed as root" 
				treeTable selectNodePath: nodes first complexContents path ] ]
		ifNotEmpty: [ 
			treeTable expandPath: clickedButton selectedPath.
			treeTable scrollSelectionIntoView ]
]

{ #category : #'event handling' }
ProfilerResultsPresenter >> handleProfilingFinished [

	| toolbarButton |
	self updateResults. 
	treeTable expandAllSuchThat: [ :item | item weightToRoot asFloat closeTo: 100.0 ].
	viewModel showLeavesOnly ifFalse: [ self selectDoItNode ].
	navigationToolbar removeAllItems.
	toolbarButton := self newToolbarButtonFrom: viewModel tallyRoot.
	navigationToolbar addItem: toolbarButton.
	self selectToolbarItem: toolbarButton.
	
	treeTable takeKeyboardFocus.
	self enabled: true
]

{ #category : #'event handling' }
ProfilerResultsPresenter >> handleProfilingStarted [
	
	viewModel reset.
	treeTable resetContents.
	self enabled: false
]

{ #category : #'event handling' }
ProfilerResultsPresenter >> handleSelectionChange [
	
	self handleSelectionChange: treeTable selectedItem
]

{ #category : #'event handling' }
ProfilerResultsPresenter >> handleSelectionChange: item [

	item
		ifNil: [ 
			browseButton disable.
			showAsRootButton disable.
			codePresenter methodOrBlock: nil ]
		ifNotNil: [ 
			browseButton enable.
			showAsRootButton enable.
			codePresenter methodOrBlock: item methodOrBlock.
			navigationToolbar selectedItem ifNotNil: [ :btn | 
				btn selectedPath: item path ] ]
]

{ #category : #initialization }
ProfilerResultsPresenter >> initializeActions [

	treeTable
		whenSelectedItemChangedSend: #handleSelectionChange
		to: self.

	"Report"
	reportButton action: [ 
		UIManager default edit: viewModel report label: 'Profiling Report' ].
	"Configuration section actions"
	collapseAllButton action: [ 
		treeTable collapseAll.
		treeTable takeKeyboardFocus ].

	expandAllButton action: [ 
		treeTable expandAll.
		treeTable takeKeyboardFocus ].

	thresholdExpandInput whenTextChangedDo: [ :text | 
		text asInteger ifNotNil: [ treeTable expandNodesOver: text asNumber ] ].

	thresholdFilterInput whenTextChangedDo: [ :text | 
		text asInteger ifNotNil: [ treeTable hideNodesUnder: text asNumber ] ].

	treeDropList whenSelectedItemChangedDo: [ :item | 
		viewModel showLeavesOnly: item value.
		self updateResults.
		viewModel showLeavesOnly ifFalse: [ 
			treeTable expandAllSuchThat: [ :node | 
				node weightToRoot asFloat closeTo: 100.0 ] ] ]
]

{ #category : #initialization }
ProfilerResultsPresenter >> initializePresenters [

	| h |
	h := self class toolbarHeight.

	self layout: (SpBoxLayout newTopToBottom
			 add: (navigationToolbar := self newProfilerToolbar) height: h;
			 add: (treeTable := self newProfilerTreeTable);
			 add: self newConfigurationSectionLayout height: h * 2.5;
			 add: (codePresenter := self newProfilerCode);
			 add: (SpBoxLayout newLeftToRight
					  add: (reportButton := self newReportButton)
					  withConstraints: [ :constraints | 
						  constraints
							   bePlacedAtStart;
							   fill: true;
							   expand: false ];
					  addLast: (showAsRootButton := self newShowAsRootButton);
					  addLast: (browseButton := self newBrowseButton))
			 height: h;
			 yourself).
	self initializeActions
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newBrowseButton [

	^ self newButton
		  icon: (self iconNamed: #glamorousBrowse);
		  label: 'Browse';
		  disable;
		  action: [ treeTable selectedItem browseItem ];
		  yourself
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newConfigurationSectionLayout [

	^ SpBoxLayout newTopToBottom
		  add: (SpBoxLayout newLeftToRight
				   add: (treeDropList := self newTreeVisualizationDropList);
				   add: (expandAllButton := self newButton
						                       label: 'Expand All';
						                       icon: (self iconNamed: #windowMenu))
				   width: 100;
				   add: (collapseAllButton := self newButton
						                         label: 'Collapse All';
						                         icon: (self iconNamed: #windowMinimize))
				   width: 100;
				   yourself);
		  add: (SpBoxLayout newLeftToRight
				   add: (self newLabel label: 'Hide nodes under: ') width: 125;
				   add: (thresholdFilterInput := self newThresholdFilterInput)
				   width: 55;
				   add: (self newLabel label: '%') width: 25;
				   yourself);
		  add: (SpBoxLayout newLeftToRight
				   add: (self newLabel label: 'Expand nodes over: ') width: 125;
				   add: (thresholdExpandInput := self newThresholdExpandInput)
				   width: 55;
				   add: (self newLabel label: '%') width: 25;
				   yourself);
		  yourself
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newProfilerCode [

	^ self instantiate: ProfilerCodePresenter 
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newProfilerNumberInput [
	^ self instantiate: ProfilerNumberInput 
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newProfilerToolbar [

	^ self instantiate: ProfilerToolbarPresenter 
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newProfilerTreeTable [

	^ ProfilerTreeTablePresenter on: viewModel
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newReportButton [

	^ self newButton
		icon: (self iconNamed: #recentMessages);
		label: 'Full Report';
		disable;
		yourself
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newShowAsRootButton [

	^ self newButton
		  icon: (self iconNamed: #glamorousRedo);
		  label: 'Show node as root';
		  action: [ viewModel setTallyRoot: treeTable selectedItem ];
		  disable;
		  yourself
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newThresholdExpandInput [

	^ self newProfilerNumberInput
		  help: 'Only nodes with a percentage of time greater than this threshold will be expanded';
		  number: 0;
		  yourself
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newThresholdFilterInput [

	^ self newProfilerNumberInput
		  help: 'Only nodes with a percentage of time greater than this threshold will be shown';
		  number: viewModel showNodeThreshold;
		  yourself
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newToolbarButtonFrom: anItem [

	| button |
	button := ProfilerToolbarButtonPresenter new.
	button
		object: anItem;
		label: anItem methodName;
		selectedPath: anItem path;
		help: 'Display ' , anItem methodName , ' as root';
		action: [ 
			viewModel setTallyRoot: anItem.
			treeTable expandPath: button selectedPath.
			treeTable scrollSelectionIntoView ].

	^ button
]

{ #category : #'private - factory' }
ProfilerResultsPresenter >> newTreeVisualizationDropList [
	| items |
	items := { 
		         ('Full tree' -> false).
		         ('Leaves only' -> true) }.

	^ self newDropList
		                help: 'Results visualization mode';
		                items: items;
		                selectItem: (viewModel showLeavesOnly
				                 ifTrue: [ items at: 2 ]
				                 ifFalse: [ items at: 1 ]);
		                display: [ :assoc | assoc key ];
		                yourself.
]

{ #category : #private }
ProfilerResultsPresenter >> removeToolbarItemsAfter: anItem [
	| toKeep item i |
	toKeep := #() asOrderedCollection.
	i := 1.
	[ 
	item := navigationToolbar items at: i.
	toKeep add: item.
	i := i + 1.
	item isNotNil and: [ item = anItem ] ] whileFalse.
	navigationToolbar removeAllItems.
	toKeep do: [ :itm | navigationToolbar addItem: itm ]
]

{ #category : #private }
ProfilerResultsPresenter >> selectDoItNode [

	| selectorsPath doItNode |
	selectorsPath := viewModel doItSelectorsPath.

	treeTable expandAllSuchThat: [ :item | 
		| bool selector |
		selector := item methodName.
		bool := (selectorsPath indexOf: selector) > 0.
		bool ifTrue: [ doItNode := item ].
		bool ].
	doItNode ifNil: [ 
		self inform:
			'No measurements were taken because excecution time was too short.'.
		^ self ].
	treeTable selectNodePath: doItNode path
]

{ #category : #private }
ProfilerResultsPresenter >> selectToolbarItem: anItem [
	| items | 
	navigationToolbar items do: [:itm | itm icon: nil ].
	anItem icon: (self iconNamed: 'windowMenu').
	
	items := navigationToolbar items.
	
	"ugly hack to force the ui refresh"
	navigationToolbar removeAllItems.
	items do: [ :itm | navigationToolbar addItem: itm ].
	navigationToolbar selectedItem: anItem.
	
]

{ #category : #'accessing - model' }
ProfilerResultsPresenter >> setModelBeforeInitialization: aDomainObject [

	viewModel := aDomainObject
]

{ #category : #subscription }
ProfilerResultsPresenter >> subscribeOnProfilingAnnouncements [

	viewModel announcer
		when: ProfilingStartedAnnouncement 
		do: [ self handleProfilingStarted ];
		when: ProfilingFinishedAnnouncement
		do: [ self handleProfilingFinished ];
		when: ProfilerResultsTreeRootChangedAnnouncement 
		do: [ :evt | self goInto: evt item ]
]

{ #category : #subscription }
ProfilerResultsPresenter >> unsubscribeFromProfilingAnnouncements [

	viewModel announcer unsubscribe: self
]

{ #category : #update }
ProfilerResultsPresenter >> updateResults [

	treeTable updateResults.
	codePresenter methodOrBlock:
		(treeTable selectedItem ifNotNil: [ :item | item methodOrBlock ])
]

{ #category : #accessing }
ProfilerResultsPresenter >> viewModel [

	^ viewModel
]
