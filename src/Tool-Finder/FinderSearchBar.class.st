"
I am a widget for the ̀Finder̀ tool. I contain all widgets regarding the search bar.
"
Class {
	#name : #FinderSearchBar,
	#superclass : #SpPresenter,
	#instVars : [
		'searchInput',
		'searchButton',
		'searchModeDropList',
		'helpButton',
		'enableRegexpCheckBox',
		'chosenPackagesButton',
		'allPackagesButton'
	],
	#category : #'Tool-Finder-Wigdets'
}

{ #category : #'updating - widgets' }
FinderSearchBar >> activateAllPackages [

	allPackagesButton iconName: #radioButtonSelected.
	chosenPackagesButton iconName: #radioButtonUnselected
]

{ #category : #'updating - widgets' }
FinderSearchBar >> activateChosenPackages [

	allPackagesButton iconName: #radioButtonUnselected.
	chosenPackagesButton iconName: #radioButtonSelected
]

{ #category : #accessing }
FinderSearchBar >> allPackagesButton [

	^ allPackagesButton
]

{ #category : #accessing }
FinderSearchBar >> chosenPackagesButton [

	^ chosenPackagesButton
]

{ #category : #initialization }
FinderSearchBar >> connectPresenters [

	helpButton action: [
		self flag: #todo. "Make better help text dialog."
		(self newText
			 beNotEditable;
			 withoutEditionContextMenu;
			 text: self owner class defaultExplanation;
			 openDialog) title: 'Finder Help' ]
]

{ #category : #layout }
FinderSearchBar >> defaultLayout [

	^ SpBoxLayout newTopToBottom
				   spacing: 2;
				   add: (SpBoxLayout newLeftToRight
						    spacing: 2;
						    add: searchModeDropList expand: false;
						    add: searchInput;
						    yourself);
				   add: (SpBoxLayout newLeftToRight
						    spacing: 2;
						    add: allPackagesButton expand: false;
						    add: chosenPackagesButton expand: false;
						    add: enableRegexpCheckBox expand: false;
						    addLast: helpButton;
						    yourself);
				   yourself
]

{ #category : #accessing }
FinderSearchBar >> enableRegexpCheckBox [

	^ enableRegexpCheckBox
]

{ #category : #accessing }
FinderSearchBar >> helpButton [

	^ helpButton
]

{ #category : #initialization }
FinderSearchBar >> initializePresenters [

	searchInput := self newSearchInput
		               placeholder: 'Search... Hit return to accept';
		               yourself.
	searchButton := self newButton label: 'Search'.
	enableRegexpCheckBox := self newCheckBox label: 'Regexp'.
	searchModeDropList := self newDropList
		                      display: [ :search | search name ];
		                      yourself.
	chosenPackagesButton := self newButton
		                        label: 'Packages…';
		                        iconName: #radioButtonUnselected;
		                        yourself.
	allPackagesButton := self newButton
		                     label: 'All Packages';
		                     iconName: #radioButtonSelected;
		                     yourself.
	helpButton := self newButton
		              icon: (Smalltalk ui icons iconNamed: #help);
		              yourself
]

{ #category : #accessing }
FinderSearchBar >> searchButton [

	^ searchButton
]

{ #category : #accessing }
FinderSearchBar >> searchInput [

	^ searchInput
]

{ #category : #accessing }
FinderSearchBar >> searchModeDropList [

	^ searchModeDropList
]

{ #category : #'updating - widgets' }
FinderSearchBar >> searchModes: aListOfSearches [

	searchModeDropList items: aListOfSearches
]

{ #category : #'updating - widgets' }
FinderSearchBar >> updateSearchModes [

	searchModeDropList items: self owner model availableSearches
]

{ #category : #events }
FinderSearchBar >> whenAllPackagesSelectedDo: aBlock [

	allPackagesButton action: aBlock
]

{ #category : #events }
FinderSearchBar >> whenPackagesSelectedDo: aBlock [

	chosenPackagesButton action: aBlock
]

{ #category : #events }
FinderSearchBar >> whenRegexActivatedDo: aBlock [

		enableRegexpCheckBox whenActivatedDo: aBlock
]

{ #category : #events }
FinderSearchBar >> whenRegexDeactivatedDo: aBlock [

	enableRegexpCheckBox whenDeactivatedDo: aBlock
]

{ #category : #events }
FinderSearchBar >> whenSearchModeChangedDo: aOneArgumentBlock [
	"aOneArgumentBlock gets the new search mode passed as the argument."

	searchModeDropList whenSelectedItemChangedDo: aOneArgumentBlock
]

{ #category : #events }
FinderSearchBar >> whenSubmitSearchDo: aOneArgumentBlock [
	"aOneArgumentBlock gets the search text passed as the argument."

	searchInput whenSubmitDo: aOneArgumentBlock
]
