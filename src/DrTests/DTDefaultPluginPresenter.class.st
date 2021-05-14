Class {
	#name : #DTDefaultPluginPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'packagesList',
		'resultsList',
		'itemsList',
		'resultViewsDropList',
		'resultLabel',
		'plugin',
		'drTests',
		'lastResults',
		'startButton'
	],
	#category : #'DrTests-Spec'
}

{ #category : #commands }
DTDefaultPluginPresenter class >> buildCommandsGroupWith: presenterInstance forRoot: rootCommandGroup [

	rootCommandGroup
		register: (self buildContextualPackageGroupWith: presenterInstance);
		register: (self buildItemsListGroupWith: presenterInstance);
		register: (self buildResultGroupWith: presenterInstance)
]

{ #category : #'private - commands' }
DTDefaultPluginPresenter class >> buildContextualPackageGroupWith: presenterInstance [

	^ (CmCommandGroup named: 'Package menu') asSpecGroup
		register: (self buildPackageGroupWith: presenterInstance);
		yourself
]

{ #category : #'private - commands' }
DTDefaultPluginPresenter class >> buildItemsListGroupWith: presenterInstance [

	^ (CmCommandGroup named: 'List items menu') asSpecGroup
		register: (DTBrowseSelectedItemCommand forSpec context: presenterInstance);
		yourself
]

{ #category : #'private - commands' }
DTDefaultPluginPresenter class >> buildPackageGroupWith: presenterInstance [

	|group|
	group := (CmCommandGroup named: 'Package tools') asSpecGroup.
	group description: 'Commands related to packages list.'.
	
	DTPackagesCommand allSubclasses do: [:each | 
		group register: (each forSpec context: presenterInstance) ].
	
	^group 
		beDisplayedAsGroup;
		yourself 
		
		
]

{ #category : #'private - commands' }
DTDefaultPluginPresenter class >> buildResultGroupWith: presenterInstance [

	| commandGroup pluginCommands |
	commandGroup := (CmCommandGroup named: 'Results tools') asSpecGroup
		description: 'Commands related to result.';
		register:
			(DTResultBrowseCommand forSpec context: presenterInstance)
				beHiddenWhenCantBeRun;
		yourself.
	pluginCommands := presenterInstance plugin
		buildContextualMenuGroupWith: presenterInstance.
	pluginCommands entries ifEmpty: [ ^ commandGroup ].
	^ commandGroup
		register: pluginCommands beDisplayedAsGroup;
		yourself
]

{ #category : #specs }
DTDefaultPluginPresenter class >> defaultSpec [

	^ SpBoxLayout newTopToBottom 
		add: (SpPanedLayout newHorizontal
		  add: (SpPanedLayout newHorizontal
				   add: (SpBoxLayout newVertical
						    add: #packagesList;
						    yourself);
				   add: (SpBoxLayout newVertical
						    add: #itemsList;
						    yourself);
				   yourself);
		  add: (SpBoxLayout newVertical
				   add: #resultViewsDropList
				   withConstraints: [ :constraints | constraints expand: false ];
				   add: #resultLabel
				   withConstraints: [ :constraints | 
					   constraints height: self labelHeight ];
				   add: #resultsList;
				   yourself);
		  yourself);
		add: #startButton
			withConstraints: [ :constraints | constraints width: self buttonWidth ]
]

{ #category : #actions }
DTDefaultPluginPresenter >> browseSelectedItem [
	"Because of DTBrowseSelectedItem>>#canBeExecuted, we know there is a single item in the selection.
	 Thus, we take the first one and browse it.
	"
	
	self selectedItems first drTestsBrowse
]

{ #category : #actions }
DTDefaultPluginPresenter >> browseSelectedResult [

	self resultSelected drTestsBrowse
]

{ #category : #utilities }
DTDefaultPluginPresenter >> buildLabelString: nameOfItems numberOfItemsSelected: aInt [
	^ String
		streamContents: [ :s | 
			s
				<< nameOfItems;
				nextPut: $(;
				<< aInt asString;
				<< ' selected):' ]
]

{ #category : #initialization }
DTDefaultPluginPresenter >> connectPresenters [

	super connectPresenters.
	
	self resultViewsDropList
		whenSelectedItemChangedDo: [ :resultTreeViewOrNil | 
			resultTreeViewOrNil
				ifNotNil: [ resultsList
						roots: (resultTreeViewOrNil resultTreeFor: drTests pluginResult) subResults ] ].

]

{ #category : #'private - initialization' }
DTDefaultPluginPresenter >> initializeItemsListAndLabel [

	itemsList := self newFilterableListPresenter.
	itemsList
		help: 'Select the classes to analyze. Cmd+A or Ctrl+A to select all classes.';
		sortingBlock: #name ascending;
		displayBlock: [ :item | item name ];
		whenSelectionChangedDo: [ self whenItemsSelectionChanged: self selectedItems ];
		beMultipleSelection;
		contextMenu: [ (self rootCommandsGroup / 'List items menu') beRoot asMenuPresenter ].
	packagesList
		whenSelectionChangedDo: [ self whenPackagesSelectionChanged: self packagesSelected ]
]

{ #category : #'private - initialization' }
DTDefaultPluginPresenter >> initializePackagesListAndLabel [

	packagesList := self newFilterableListPresenter.
	packagesList
		help: 'Select the packages to analyze. Cmd+A or Ctrl+A to select all packages.';
		sortingBlock: #name ascending;
		displayBlock: [ :package | package name ];
		beMultipleSelection;
		contextMenu: [ (self rootCommandsGroup / 'Package menu') beRoot asMenuPresenter ]
]

{ #category : #initialization }
DTDefaultPluginPresenter >> initializePresenters [

	super initializePresenters.
		
	self
		initializePackagesListAndLabel;
		initializeItemsListAndLabel;
		initializeResultsTreeAndLabel;
		initializeResultViewsDropList.
		
	startButton := self newButton.
	startButton action: [ drTests runPlugin ]
]

{ #category : #'private - initialization' }
DTDefaultPluginPresenter >> initializeResultViewsDropList [

	resultViewsDropList := self newDropList.
	self resultViewsDropList
		help: 'Select the different views for results';
		display: [ :resultTreeView | resultTreeView name ]
]

{ #category : #'private - initialization' }
DTDefaultPluginPresenter >> initializeResultsTreeAndLabel [

	resultLabel := self newLabel label: 'Results:'.
	resultsList := self newTreeTable.
	resultsList
		addColumn: (SpStringTableColumn evaluated: #drTestsName);
		children: [ :node | node subResults ];
		contextMenu: [ (self rootCommandsGroup / 'Results tools') beRoot asMenuPresenter ].

	self resultTree: DTTreeNode empty.

	
]

{ #category : #accessing }
DTDefaultPluginPresenter >> itemsList [
	^ itemsList
]

{ #category : #widgets }
DTDefaultPluginPresenter >> newFilterableListPresenter [

	^ self instantiate: DTFilterableListPresenter
]

{ #category : #accessing }
DTDefaultPluginPresenter >> packagesList [
	^ packagesList
]

{ #category : #private }
DTDefaultPluginPresenter >> packagesSelected [

	^ self packagesList ifNil: [ #(  ) ] ifNotNil: #selectedItems
]

{ #category : #accessing }
DTDefaultPluginPresenter >> plugin [
	^ plugin
]

{ #category : #accessing }
DTDefaultPluginPresenter >> resultSelected [

	^ self resultsList selectedItem
]

{ #category : #api }
DTDefaultPluginPresenter >> resultTree: aResultGroup [

	aResultGroup
		ifNotNil: [ resultsList roots: aResultGroup subResults ]
]

{ #category : #accessing }
DTDefaultPluginPresenter >> resultViewsDropList [
	^ resultViewsDropList
]

{ #category : #accessing }
DTDefaultPluginPresenter >> resultsList [
	^ resultsList
]

{ #category : #api }
DTDefaultPluginPresenter >> selectAllInPackageList [

	packagesList selectAll
]

{ #category : #api }
DTDefaultPluginPresenter >> selectNoneInPackageList [

	packagesList unselectAll
]

{ #category : #accessing }
DTDefaultPluginPresenter >> selectedItems [

	^ itemsList selectedItems
]

{ #category : #accessing }
DTDefaultPluginPresenter >> setModelBeforeInitialization: aPair [

	plugin := aPair first.
	drTests := aPair second
]

{ #category : #accessing }
DTDefaultPluginPresenter >> startButton [
	^ startButton
]

{ #category : #accessing }
DTDefaultPluginPresenter >> subwidgetsToLock [
	
	^ { packagesList. itemsList. resultsList. startButton}
]

{ #category : #'private - updating' }
DTDefaultPluginPresenter >> updateItemsListLabel [

	itemsList label: (self
			 buildLabelString: plugin secondListLabel
			 numberOfItemsSelected: self selectedItems size)
]

{ #category : #'private - updating' }
DTDefaultPluginPresenter >> updatePackagesList [
	packagesList unselectAll.
	itemsList beEmpty.
	packagesList items: plugin packagesAvailableForAnalysis.
	packagesList label: plugin firstListLabel.
	itemsList label: plugin secondListLabel.
	plugin setSelectionModeOfPackageList: packagesList.
	plugin setSelectionModeOfItemsList: itemsList
]

{ #category : #'private - updating' }
DTDefaultPluginPresenter >> updatePackagesListLabel [
	self packagesList
		label:
			(self
				buildLabelString: plugin firstListLabel
				numberOfItemsSelected: self packagesSelected size)
]

{ #category : #initialization }
DTDefaultPluginPresenter >> updatePresenter [

	super updatePresenter.

	self updatePackagesList.
	self updateResultViewsDropList.
	
	startButton label: plugin startButtonLabel.
	startButton help: plugin startButtonHelp.
]

{ #category : #updating }
DTDefaultPluginPresenter >> updateResultLabel [
	resultLabel
		label: 'Results: ■';
		color: lastResults backgroundColor
]

{ #category : #updating }
DTDefaultPluginPresenter >> updateResultViewsDropList [
	| newPragmas |
	newPragmas := plugin resultTreeViews.
	
	(resultViewsDropList listItems = newPragmas
		and: [ newPragmas isNotEmpty ])
		ifTrue: [ 
			"Trigger action attached to selection change."
			resultViewsDropList selectedIndex: resultViewsDropList selectedIndex.
			^ self ].
	
	self resultViewsDropList selectedItem
		ifNotNil: [ self resultViewsDropList resetSelection ].
	self resultViewsDropList items: newPragmas.
	newPragmas isNotEmpty
		ifTrue: [ self resultViewsDropList selectIndex: 1 ]
]

{ #category : #updating }
DTDefaultPluginPresenter >> updateWithResults: someResults [
	lastResults := someResults.

	self resultTree: lastResults buildTreeForUI.
	self updateResultViewsDropList.
	self updateResultLabel.
	resultsList
		contextMenu: [ (self rootCommandsGroup / 'Results tools') beRoot asMenuPresenter ]
]

{ #category : #private }
DTDefaultPluginPresenter >> whenItemsSelectionChanged: itemsSelected [
	
	self updateItemsListLabel.
	drTests updateSwitchButton: itemsSelected
]

{ #category : #private }
DTDefaultPluginPresenter >> whenPackagesSelectionChanged: packagesSelected [

	itemsList
		items: (plugin itemsToBeAnalysedFor: packagesSelected);
		selectAll.
	self updatePackagesListLabel
]
