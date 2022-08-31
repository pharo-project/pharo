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

{ #category : #private }
ProfilerResultsPresenter >> goInto: aTallyModel [

	| maybeClickedButton clickedButton |
	maybeClickedButton := navigationToolbar getItemWith: aTallyModel.
	"Either 
		- The button must be created  
			(ie, 'see as root' in the context menu was clicked)
		- Or it already exists 
			(ie, the toolbar button was clicked)"
	maybeClickedButton
		ifNil: [ 
			self removeToolbarItemsAfter: navigationToolbar selectedItem.
			clickedButton := self newToolbarButtonFrom: aTallyModel.
			navigationToolbar addItem: clickedButton ]
		ifNotNil: [ 
			clickedButton := maybeClickedButton
			"(clickedButton == navigationToolbar selectedItem and: [ 
				 treeTable roots isNotEmpty ]) ifTrue: [ ^ self ]" ].

	clickedButton click
]

{ #category : #'event handling' }
ProfilerResultsPresenter >> handleProfilingFinished [

	| doItNode |
	self enabled: true.

	navigationToolbar selectedItem: nil.
	navigationToolbar removeAllItems.

	self goInto: viewModel tallyRoot.

	viewModel showLeavesOnly ifTrue: [ ^ self ].

	"select do it node and expand tree"
	doItNode := self doItNode.

	doItNode ifNil: [ 
		self inform:
			'The excecution time was too short! No measurements taken'.
		^ self ].

	treeTable emptySelection.
	treeTable selectNodePath: doItNode path.
	"self goInto: doItNode.
	treeTable selectNodePath:
		(doItNode pathStartingFrom: viewModel tallyRoot path)"
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
				btn selectedPath: (item pathStartingFrom: viewModel tallyRoot path) ] ]
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
		treeTable roots: viewModel rootItems.
		viewModel showLeavesOnly ifFalse: [ 
			treeTable expandAllSuchThat: [ :node | 
				node weightToRoot asFloat closeTo: 100.0 ] ] ]
]

{ #category : #initialization }
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
ProfilerResultsPresenter >> newToolbarButtonFrom: aTallyModel [

	| button pathToSelect |
	button := ProfilerToolbarButtonPresenter new.
	pathToSelect := aTallyModel == viewModel tallyRoot
		                ifTrue: [ [ self doItNode path ] ]
		                ifFalse: [ [ aTallyModel path ] ].
	button
		object: aTallyModel;
		label: aTallyModel methodName;
		selectedPath: pathToSelect;
		help: 'Display ' , aTallyModel methodName , ' as root';
		action: [ 
			button == navigationToolbar selectedItem ifFalse: [ 
					navigationToolbar setSelectedItem: button.
					viewModel setTallyRoot: aTallyModel ] ].

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

{ #category : #'accessing - model' }
ProfilerResultsPresenter >> setModelBeforeInitialization: aDomainObject [

	viewModel := aDomainObject
]

{ #category : #subscription }
ProfilerResultsPresenter >> subscribeOnProfilingAnnouncements [

	viewModel announcer
		when: ProfilingStartedAnnouncement do: [ self enabled: false ];
		when: ProfilingFinishedAnnouncement do: [ 
			self handleProfilingFinished.
			viewModel announcer announce:
					ProfilingResultsDisplayedAnnouncement new ];
		when: ProfilerResultsTreeRootChangedAnnouncement do: [ :evt | 
			treeTable roots: viewModel rootItems.
			treeTable expandAllSuchThat: [ :tallyModel | 
					tallyModel weightToRoot asFloat closeTo: 100.0 ].
			treeTable expandPath: evt item path.
			treeTable selectNodePath: evt item path.
			treeTable scrollSelectionIntoView ]
]

{ #category : #subscription }
ProfilerResultsPresenter >> unsubscribeFromProfilingAnnouncements [

	viewModel announcer unsubscribe: self
]

{ #category : #accessing }
ProfilerResultsPresenter >> viewModel [

	^ viewModel
]
