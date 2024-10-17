Class {
	#name : 'DTDefaultPluginPresenter',
	#superclass : 'SpPresenter',
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
	#category : 'DrTests-Spec',
	#package : 'DrTests',
	#tag : 'Spec'
}

{ #category : 'commands' }
DTDefaultPluginPresenter class >> buildCommandsGroupWith: presenterInstance forRoot: rootCommandGroup [

	rootCommandGroup
		register: (self buildContextualPackageGroupWith: presenterInstance);
		register: (self buildItemsListGroupWith: presenterInstance);
		register: (self buildResultGroupWith: presenterInstance)
]

{ #category : 'private - commands' }
DTDefaultPluginPresenter class >> buildContextualPackageGroupWith: presenterInstance [

	^ (CmCommandGroup named: 'Package menu') asSpecGroup
		register: (self buildPackageGroupWith: presenterInstance);
		yourself
]

{ #category : 'private - commands' }
DTDefaultPluginPresenter class >> buildItemsListGroupWith: presenterInstance [

	^ (CmCommandGroup named: 'List items menu') asSpecGroup
		register: (DTBrowseSelectedItemCommand forSpec context: presenterInstance);
		yourself
]

{ #category : 'private - commands' }
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

{ #category : 'private - commands' }
DTDefaultPluginPresenter class >> buildResultGroupWith: presenterInstance [
	| commandGroup pluginCommands plugin |

	commandGroup := (CmCommandGroup named: 'Results tools') asSpecGroup
		description: 'Commands related to result.';
		register: (DTResultBrowseCommand forSpecContext: presenterInstance) beHiddenWhenCantBeRun;
		yourself.

	plugin := presenterInstance plugin.
	plugin ifNil: [ ^ commandGroup ].

	pluginCommands := plugin buildContextualMenuGroupWith: presenterInstance.
	pluginCommands entries ifEmpty: [ ^ commandGroup ].
	^ commandGroup
		register: pluginCommands beDisplayedAsGroup;
		yourself
]

{ #category : 'actions' }
DTDefaultPluginPresenter >> browseSelectedItem [
	"Because of DTBrowseSelectedItem>>#canBeExecuted, we know there is a single item in the selection.
	 Thus, we take the first one and browse it.
	"

	self selectedItems first drTestsBrowse
]

{ #category : 'actions' }
DTDefaultPluginPresenter >> browseSelectedResult [

	self resultSelected drTestsBrowse
]

{ #category : 'private' }
DTDefaultPluginPresenter >> buildLabelString: nameOfItems numberOfItemsSelected: aInt [

	^ String streamContents: [ :stream |
		stream
			<< nameOfItems;
			<< ' (';
			<< aInt asString;
			<< ' selected):' ]
]

{ #category : 'initialization' }
DTDefaultPluginPresenter >> connectPresenters [

	super connectPresenters.

	self resultViewsDropList
		whenSelectedItemChangedDo: [ :resultTreeViewOrNil |
			resultTreeViewOrNil
				ifNotNil: [ resultsList
						roots: (resultTreeViewOrNil resultTreeFor: drTests pluginResult) subResults ] ]
]

{ #category : 'layout' }
DTDefaultPluginPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		add: (SpPanedLayout newLeftToRight
			add: (SpPanedLayout newLeftToRight
		  		add: packagesList;
				add: itemsList;
				yourself);
			add: (SpBoxLayout newTopToBottom
				spacing: 5;
				add: resultViewsDropList expand: false;
				add: resultLabel expand: false;
				add: resultsList;
				yourself);
		  yourself);
		add: startButton expand: false;
		yourself
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> drTests [
	^ drTests
]

{ #category : 'initialization' }
DTDefaultPluginPresenter >> initializeItemsListAndLabel [

	itemsList := self newFilterableListPresenter.
	itemsList
		displayIcon: [ :aClass | self iconNamed: aClass systemIconName  ];
		displayColor: [ :aClass |
			(self packagesSelected includes: aClass package)
				ifTrue: [ self theme textColor ]
				ifFalse: [ self theme classExtensionColor ] ];
		help: 'Select the classes to analyze. Cmd+A or Ctrl+A to select all classes.';
		sortingBlock: #name ascending;
		displayBlock: [ :item | item name ];
		whenSelectionChangedDo: [ self whenItemsSelectionChanged: self selectedItems ];
		beMultipleSelection;
		actions: (self rootCommandsGroup / 'List items menu') beRoot.

	packagesList
		whenSelectionChangedDo: [ self whenPackagesSelectionChanged: self packagesSelected ]
]

{ #category : 'initialization' }
DTDefaultPluginPresenter >> initializePackagesListAndLabel [

	packagesList := self newFilterableListPresenter.
	packagesList
		help: 'Select the packages to analyze. Cmd+A or Ctrl+A to select all packages.';
		sortingBlock: #name ascending;
		displayBlock: [ :package | package name ];
		beMultipleSelection;
		actions: self rootCommandsGroup / 'Package menu'
]

{ #category : 'initialization' }
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

{ #category : 'initialization' }
DTDefaultPluginPresenter >> initializeResultViewsDropList [

	resultViewsDropList := self newDropList.
	self resultViewsDropList
		help: 'Select the different views for results';
		display: [ :resultTreeView | resultTreeView name ]
]

{ #category : 'initialization' }
DTDefaultPluginPresenter >> initializeResultsTreeAndLabel [

	resultLabel := self newLabel label: 'Results:'.
	resultsList := self newTree.
	resultsList
		display: [ :node | node drTestsName ];
		displayColor: [ :node | node displayColor ];
		children: [ :node | node subResults ];
		actions: self rootCommandsGroup / 'Results tools'.

	resultsList outputActivationPort
		transmitDo: [
			(DTResultBrowseCommand forSpecContext: self) execute ]
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> itemsList [
	^ itemsList
]

{ #category : 'widgets' }
DTDefaultPluginPresenter >> newFilterableListPresenter [

	^ self instantiate: DTFilterableListPresenter
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> packagesList [
	^ packagesList
]

{ #category : 'private' }
DTDefaultPluginPresenter >> packagesSelected [

	^ self packagesList ifNil: [ #(  ) ] ifNotNil: #selectedItems
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> plugin [
	^ plugin
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> resultSelected [

	^ self resultsList selectedItem
]

{ #category : 'api' }
DTDefaultPluginPresenter >> resultTree: aResultGroup [

	| roots |
	aResultGroup ifNil: [ ^ self ].
	roots := aResultGroup subResults.
	resultsList roots: roots.
	roots withIndexDo: [ :each :index |
		each shouldStartExpanded ifTrue: [
			"here there was a each name crTrace. 
			It does not look like the good way to provide feedback in the transcript"
			resultsList expandPath: { index } ] ]
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> resultViewsDropList [
	^ resultViewsDropList
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> resultsList [
	^ resultsList
]

{ #category : 'api' }
DTDefaultPluginPresenter >> selectAllInPackageList [

	packagesList selectAll
]

{ #category : 'api' }
DTDefaultPluginPresenter >> selectNoneInPackageList [

	packagesList unselectAll
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> selectedItems [

	^ itemsList selectedItems
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> setModelBeforeInitialization: aPair [

	plugin := aPair first.
	drTests := aPair second
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> startButton [
	^ startButton
]

{ #category : 'accessing' }
DTDefaultPluginPresenter >> subwidgetsToLock [

	^ { packagesList. itemsList. resultsList. startButton}
]

{ #category : 'private - updating' }
DTDefaultPluginPresenter >> updateItemsListLabel [

	itemsList label: (self
		 buildLabelString: plugin secondListLabel
		 numberOfItemsSelected: self selectedItems size)
]

{ #category : 'private - updating' }
DTDefaultPluginPresenter >> updatePackagesList [

	packagesList unselectAll.
	itemsList beEmpty.
	packagesList items: plugin packagesAvailableForAnalysis.
	packagesList label: plugin firstListLabel.
	itemsList label: plugin secondListLabel.
	plugin setSelectionModeOfPackageList: packagesList.
	plugin setSelectionModeOfItemsList: itemsList
]

{ #category : 'private - updating' }
DTDefaultPluginPresenter >> updatePackagesListLabel [

	self packagesList label: (self
		buildLabelString: plugin firstListLabel
		numberOfItemsSelected: self packagesSelected size)
]

{ #category : 'initialization' }
DTDefaultPluginPresenter >> updatePresenter [

	super updatePresenter.

	self updatePackagesList.
	self updateResultViewsDropList.

	startButton label: plugin startButtonLabel.
	startButton help: plugin startButtonHelp
]

{ #category : 'updating' }
DTDefaultPluginPresenter >> updateResultLabel [

	resultLabel styles copy
		do: [ :each | resultLabel removeStyle: each ].
	resultLabel
		label: 'Results:';
		addStyle: lastResults backgroundColorStyle
]

{ #category : 'updating' }
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

{ #category : 'updating' }
DTDefaultPluginPresenter >> updateWithResults: someResults [

	lastResults := someResults.

	self updateResultViewsDropList.
	self updateResultLabel.
	self resultTree: lastResults buildTreeForUI.
	resultsList actions: self rootCommandsGroup / 'Results tools'
]

{ #category : 'private' }
DTDefaultPluginPresenter >> whenItemsSelectionChanged: itemsSelected [

	self updateItemsListLabel.
	drTests updateSwitchButton: itemsSelected
]

{ #category : 'private' }
DTDefaultPluginPresenter >> whenPackagesSelectionChanged: packagesSelected [

	itemsList
		items: (plugin itemsToBeAnalysedFor: packagesSelected);
		selectAll.
	self updatePackagesListLabel
]
