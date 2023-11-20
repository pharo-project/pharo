"
I am a panel in the ProfilerPresenter containing the profiling process results together with parameters to filter these results
"
Class {
	#name : 'ProfilerResultsPresenter',
	#superclass : 'SpPresenter',
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
	#category : 'ProfilerUI-View',
	#package : 'ProfilerUI',
	#tag : 'View'
}

{ #category : 'private' }
ProfilerResultsPresenter >> doItNode [

	| selectorsPath doItNode |
	selectorsPath := viewModel doItSelectorsPath.
	doItNode := nil.
	treeTable expandAllSuchThat: [ :item |
		| bool |
		bool := (selectorsPath indexOf: item methodName) == 1 and: [
			        item hasContents ].
		bool ifTrue: [
			doItNode := item.
			selectorsPath := selectorsPath allButFirst ].
		bool ].
	(doItNode isNil or: [
		 doItNode methodName ~= viewModel doItSelectorsPath last ]) ifTrue: [
		^ nil ].

	^ doItNode
]

{ #category : 'event handling' }
ProfilerResultsPresenter >> handleProfilingFinished [

	| doItNode |
	self defer: [
		self enabled: true.

		viewModel showLeavesOnly ifFalse: [ "select do it node and expand tree"
			doItNode := self doItNode.
			doItNode
				ifNil: [
					self inform:
						'The excecution time was too short! No measurements taken' ]
				ifNotNil: [
					treeTable selectPath: doItNode path scrollToSelection: true.
					viewModel setTallyRoot: doItNode ] ].
		viewModel announcer announce:
			ProfilingResultsDisplayedAnnouncement new ]
]

{ #category : 'event handling' }
ProfilerResultsPresenter >> handleProfilingStarted [
	self enabled: false.
	navigationToolbar reset
]

{ #category : 'event handling' }
ProfilerResultsPresenter >> handleSelectionChanged [
	| navButton node |
	node := treeTable selectedItem.

	node
		ifNil: [
			browseButton disable.
			showAsRootButton disable.
			codePresenter methodOrBlock: nil ]
		ifNotNil: [
			browseButton enable.
			showAsRootButton enable.
			codePresenter methodOrBlock: node methodOrBlock.
			 ].

	navButton := navigationToolbar selectedItem.
	navButton ifNil: [ ^ self ].
	navButton selectedObject: node
]

{ #category : 'event handling' }
ProfilerResultsPresenter >> handleTreeRootChanged: event [

	| nodeToSelect pathToSelect |
	self assert: event item == viewModel tallyRoot.

	treeTable roots: viewModel rootItems.
	self assert: treeTable selectedItem isNil.

	navigationToolbar setSelectedItem:
		(navigationToolbar ensureItemWith: viewModel tallyRoot).

	nodeToSelect := navigationToolbar selectedItem
		                ifNil: [ event item ]
		                ifNotNil: [ :btn |
		                btn selectedObject ifNil: [ event item ] ].

	pathToSelect := viewModel showLeavesOnly
		                ifTrue: [ nodeToSelect path ]
		                ifFalse: [ nodeToSelect relativePath ].

	viewModel showLeavesOnly ifFalse: [
		treeTable expandAllSuchThat: [ :node |
			node weightToRoot asFloat closeTo: 100.0 ].
		treeTable expandPath: pathToSelect ].

	treeTable selectPath: pathToSelect scrollToSelection: true
]

{ #category : 'initialization' }
ProfilerResultsPresenter >> initializeActions [

	treeTable
		whenSelectedItemChangedSend: #handleSelectionChanged
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
		viewModel showLeavesOnly: item value ]
]

{ #category : 'initialization' }
ProfilerResultsPresenter >> initializePresenters [

	| h |
	h := self class toolbarHeight.

	self layout: (SpPanedLayout newTopToBottom
			 positionOfSlider: 50 percent;
			 add: (SpBoxLayout newTopToBottom
					  add: (navigationToolbar := self newProfilerToolbar) height: h;
					  add: (treeTable := self newProfilerTreeTable);
					  yourself);
			 add: (SpBoxLayout newTopToBottom
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
					  yourself);
			 yourself).

	self initializeActions
]

{ #category : 'private - factory' }
ProfilerResultsPresenter >> newBrowseButton [

	^ self newButton
		  icon: (self iconNamed: #glamorousBrowse);
		  label: 'Browse';
		  disable;
		  action: [ treeTable selectedItem browseItem ];
		  yourself
]

{ #category : 'private - factory' }
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

{ #category : 'private - factory' }
ProfilerResultsPresenter >> newProfilerCode [

	^ self instantiate: ProfilerCodePresenter
]

{ #category : 'private - factory' }
ProfilerResultsPresenter >> newProfilerNumberInput [
	^ self instantiate: ProfilerNumberInput
]

{ #category : 'private - factory' }
ProfilerResultsPresenter >> newProfilerToolbar [

	^ self instantiate: ProfilerToolbarPresenter
]

{ #category : 'private - factory' }
ProfilerResultsPresenter >> newProfilerTreeTable [

	^ ProfilerTreeTablePresenter on: viewModel
]

{ #category : 'private - factory' }
ProfilerResultsPresenter >> newReportButton [

	^ self newButton
		icon: (self iconNamed: #recentMessages);
		label: 'Full Report';
		disable;
		yourself
]

{ #category : 'private - factory' }
ProfilerResultsPresenter >> newShowAsRootButton [

	^ self newButton
		  icon: (self iconNamed: #glamorousRedo);
		  label: 'Show node as root';
		  action: [ viewModel setTallyRoot: treeTable selectedItem ];
		  disable;
		  yourself
]

{ #category : 'private - factory' }
ProfilerResultsPresenter >> newThresholdExpandInput [

	^ self newProfilerNumberInput
		  help: 'Only nodes with a percentage of time greater than this threshold will be expanded';
		  number: 0;
		  yourself
]

{ #category : 'private - factory' }
ProfilerResultsPresenter >> newThresholdFilterInput [

	^ self newProfilerNumberInput
		  help: 'Only nodes with a percentage of time greater than this threshold will be shown';
		  number: viewModel showNodeThreshold;
		  yourself
]

{ #category : 'private - factory' }
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
		                yourself
]

{ #category : 'accessing - model' }
ProfilerResultsPresenter >> setModelBeforeInitialization: aDomainObject [

	viewModel := aDomainObject
]

{ #category : 'subscription' }
ProfilerResultsPresenter >> subscribeOnProfilingAnnouncements [

	viewModel announcer
		when: ProfilingStartedAnnouncement send: #handleProfilingStarted to: self;
		when: ProfilingFinishedAnnouncement send: #handleProfilingFinished to: self;
		when: ProfilerResultsTreeRootChangedAnnouncement send: #handleTreeRootChanged: to: self
]

{ #category : 'subscription' }
ProfilerResultsPresenter >> unsubscribeFromProfilingAnnouncements [

	viewModel announcer unsubscribe: self
]

{ #category : 'accessing' }
ProfilerResultsPresenter >> viewModel [

	^ viewModel
]
