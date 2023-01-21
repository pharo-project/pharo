"
I am the presenter for the ̀Finder̀ tool.

I define the user interface of the finder tool.
"
Class {
	#name : #FinderPresenter,
	#superclass : #SpPresenterWithModel,
	#instVars : [
		'searchInput',
		'searchButton',
		'searchModeDropList',
		'searchEnableRegexpCheckBox',
		'helpButton',
		'resultTree',
		'resultBrowseButton',
		'resultSendersButton',
		'resultImplementorsButton',
		'resultVersionsButton',
		'resultInheritanceButton',
		'resultHierarchyButton',
		'searchOptionsBar',
		'resultButtonBar',
		'resultStatusBar',
		'searchChosenPackagesButton',
		'searchAllPackagesButton'
	],
	#category : #'Tool-Finder-Base'
}

{ #category : #'default values' }
FinderPresenter class >> defaultExplanation [

	^ 'The Finder can be used by default in 4 different ways:
	- Selectors: your research is done among selectors
	- Classes : your research is done among classes names
	- Source : your research is done among all the source code
	- Pragmas: your research is done among pragmas
	- Examples : your research uses the Method Finder behavior 
			   (for further informations, print ''FinderUI methodFinderExplanation'')
	
			
In these four modes, you can also tick the ''Use RegEx'' checkbox.
If you pick this box, your search will be done using regular expressions instead of just matching.

The ''Select classes'' button opened a dialog window  to select which classes will be used for the search.
The ''All classes'' button is used to reset the classes selection.'
]

{ #category : #initialization }
FinderPresenter >> connectPresenters [

	searchInput whenSubmitDo: [ :text | self model searchBy: text ].
	searchAllPackagesButton action: [ self searchInAllPackages ].
	searchChosenPackagesButton action: [ self openPackageChooserDialog ].
	searchEnableRegexpCheckBox
		whenActivatedDo: [ self model useRegex: true ];
		whenDeactivatedDo: [ self model useRegex: false ].

	helpButton action: [
		self flag: #todo. "Make better help text dialog."
		(self newText
			 beNotEditable;
			 withoutEditionContextMenu;
			 text: self class defaultExplanation;
			 openDialog) title: 'Finder Help' ].

	resultTree whenSelectedItemChangedDo: [ :newResult |
		self updateResultButtonsFor: newResult ].

	self connectResultButtons.

	self subscribeToAnnouncements
]

{ #category : #initialization }
FinderPresenter >> connectResultButtons [

	resultBrowseButton action: [
		resultTree selectedItem ifNotNil: [ :result | result browseAction ] ].

	resultSendersButton action: [
		resultTree selectedItem ifNotNil: [ :result | result sendersAction ] ].

	resultImplementorsButton action: [
		resultTree selectedItem ifNotNil: [ :result | result implementersAction ] ].

	resultVersionsButton action: [
		resultTree selectedItem ifNotNil: [ :result | result versionsAction ] ].

	resultInheritanceButton action: [
		resultTree selectedItem ifNotNil: [ :result | result inheritanceAction ] ].

	resultHierarchyButton action: [
		resultTree selectedItem ifNotNil: [ :result | result hierarchyAction ] ]
]

{ #category : #layout }
FinderPresenter >> defaultLayout [

	self flag: #todo. "Add Statusbar back again"
	^ SpBoxLayout newTopToBottom
		  spacing: 5;
		  add: (SpBoxLayout newTopToBottom
				   spacing: 2;
				   add: (SpBoxLayout newLeftToRight
						    spacing: 2;
						    add: searchModeDropList expand: false;
						    add: searchInput;
						    yourself);
				   add: (SpBoxLayout newLeftToRight
						    spacing: 2;
						    add: searchAllPackagesButton expand: false;
						    add: searchChosenPackagesButton expand: false;
						    add: searchEnableRegexpCheckBox expand: false;
						    addLast: helpButton;
						    yourself);
				   yourself)
		  expand: false;
		  add: resultTree;
		  "add: resultStatusBar expand: false;"
		  add: resultButtonBar expand: false;
		  yourself
]

{ #category : #initialization }
FinderPresenter >> initializePresenters [

	self initializeSearchBar.

	resultTree := self newTree display: [ :result | result displayString ].

	self flag: #toImplement. "StatusBar with info about nr of packages searched and results."
	self initializeStatusBar.
	self initializeResultButtonBar
]

{ #category : #initialization }
FinderPresenter >> initializeResultButtonBar [

	resultBrowseButton := self newButton disable label: 'Browse'.
	resultSendersButton := self newButton disable label: 'Senders'.
	resultImplementorsButton := self newButton disable label:
		                            'Implementors'.
	resultVersionsButton := self newButton disable label: 'Versions'.
	resultInheritanceButton := self newButton disable label:
		                           'Inheritance'.
	resultHierarchyButton := self newButton disable label: 'Hierarchy'.

	resultButtonBar := SpBoxLayout newHorizontal
		                   spacing: 2;
		                   add: resultBrowseButton expand: false;
		                   add: resultSendersButton expand: false;
		                   add: resultImplementorsButton expand: false;
		                   add: resultVersionsButton expand: false;
		                   add: resultInheritanceButton expand: false;
		                   add: resultHierarchyButton expand: false;
		                   yourself
]

{ #category : #initialization }
FinderPresenter >> initializeSearchBar [

	searchInput := self newSearchInput
		               placeholder: 'Search... Hit return to accept';
		               yourself.
	searchButton := self newButton label: 'Search'.
	searchEnableRegexpCheckBox := self newCheckBox label: 'Regexp'.
	searchModeDropList := self newDropList
		                      items: self model availableSearches;
		                      display: [ :search | search name ];
		                      whenSelectedItemChangedDo: [ :new | 
			                      self model currentSearch: new ];
		                      yourself.
	searchChosenPackagesButton := self newButton
		                              label: 'Packages…';
		                              iconName: #radioButtonUnselected;
		                              yourself.
	searchAllPackagesButton := self newButton
		                           label: 'All Packages';
		                           iconName: #radioButtonSelected;
		                           yourself.
	helpButton := self newButton
		              icon: (Smalltalk ui icons iconNamed: #help);
		              yourself
]

{ #category : #initialization }
FinderPresenter >> initializeStatusBar [

	resultStatusBar := self newStatusBar
]

{ #category : #private }
FinderPresenter >> openPackageChooserDialog [
	"Opens a `SpChooserPresenter` dialog to allow the user to choose the packages to search in."

	((SpChooserPresenter
		  sourceItems: self model unselectedPackages
		  chosenItems: self model selectedPackages)
		 sourceLabel: 'Available packages';
		 targetLabel: 'Selected packages';
		 openDialog)
		title: 'Select packages for searching';
		okAction: [ :dialog | 
		self searchInPackages: dialog presenter chosenItems ]
]

{ #category : #private }
FinderPresenter >> searchInAllPackages [

	"Selects all packages for searching."

	self model selectAllPackages.

	searchAllPackagesButton iconName: #radioButtonSelected.
	searchChosenPackagesButton iconName: #radioButtonUnselected
]

{ #category : #private }
FinderPresenter >> searchInPackages: aCollection [

	"Sets the search environment in the model to the package names given in aCollection."

	self model selectedPackagesByName: aCollection.

	searchAllPackagesButton iconName: #radioButtonUnselected.
	searchChosenPackagesButton iconName: #radioButtonSelected
]

{ #category : #initialization }
FinderPresenter >> subscribeToAnnouncements [

	self model whenSearchStarted: [  ].
	self model whenSearchEnded: [ :results |
		self updateResultsWith: results ].
	self model whenSearchTypesChanged: [ self updateSearchModes ]
]

{ #category : #'updating - widgets' }
FinderPresenter >> updateResultButtonsFor: newResult [
	"Enables or disables result action buttons depending on the defined
	 actions for the selected result."

	newResult ifNil: [ ^ self ].

	resultBrowseButton enabled: newResult hasBrowseAction.
	resultSendersButton enabled: newResult hasSendersAction.
	resultImplementorsButton enabled: newResult hasImplementersAction.
	resultVersionsButton enabled: newResult hasVersionsAction.
	resultInheritanceButton enabled: newResult hasInheritanceAction.
	resultHierarchyButton enabled: newResult hasHierarchyAction
]

{ #category : #'updating - widgets' }
FinderPresenter >> updateResults [
	"Call to update resultTree from the model."

	self updateResultsWith: self model results
]

{ #category : #'updating - widgets' }
FinderPresenter >> updateResultsWith: results [
	"Call to update resultTree with results."

	resultTree
		roots: results;
		children: [ :result | result children ]
]

{ #category : #'updating - widgets' }
FinderPresenter >> updateSearchModes [

	searchModeDropList items: self model availableSearches
]
