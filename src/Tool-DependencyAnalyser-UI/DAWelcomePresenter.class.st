"
Welcome user interface to browse dependencies packages. Availaible via tools menu.

self new openWithSpec
"
Class {
	#name : #DAWelcomePresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'packageLabel',
		'packageChosenLabel',
		'buttonOpen',
		'packageList',
		'packageChosenList',
		'buttonAddPackage',
		'buttonRemovePackage',
		'buttonAddAll',
		'buttonRemoveAll',
		'textPackageField'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #specs }
DAWelcomePresenter class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn:
			[ :col | 
			col
				newRow: [ :row | 
							row
								add: #packageLabel;
								add: #packageChosenLabel ]
					height: 30;
				newRow: [ :row | 
							row
								add: #textPackageField;
								add: #buttonOpen ]
					height: 30;
				newRow: [ :row | 
							row
								add: #packageList;
								add: #packageChosenList ];
				newRow: [ :row | 
							row
								add: #buttonAddAll;
								add: #buttonAddPackage;
								add: #buttonRemoveAll;
								add: #buttonRemovePackage ]
					height: 30 ] yourself
]

{ #category : #'world menu' }
DAWelcomePresenter class >> menuCommandOn: aBuilder [
	<worldMenu>

	(aBuilder item: #PackageDependencies)
		order: 2;
		parent: #Packaging;
		label: 'Dependency Analyser';
		icon: (self iconNamed: #packageIcon);
		help: 'Analyze dependencies between different packages in the image.';
		action: [ self new open ]
]

{ #category : #protocol }
DAWelcomePresenter >> actionOnAddPackage [
	| itemsToAdd itemsChosen itemsAdded |
	buttonAddPackage
		action: [ 
			self selectedItemsFromPackageList
				ifNotEmpty: [ 
					itemsToAdd := self selectedItemsFromPackageList asOrderedCollection.
					itemsAdded := self itemsFromPackageChosenList asOrderedCollection.
					itemsChosen := self merge: itemsToAdd With: itemsAdded.
					self packageChosenList items: itemsChosen asSortedCollection.
					self updatePackageChosenLabel ] ]
]

{ #category : #protocol }
DAWelcomePresenter >> actionOnOpen [
	buttonOpen
		action: [ 
			self itemsFromPackageChosenList
				ifNotEmpty: [ 
					(DAPackageDependenciesPresenter onPackagesNamed: self itemsFromPackageChosenList) open.
					 ] ]
]

{ #category : #protocol }
DAWelcomePresenter >> actionOnRemovePackage [
	| itemsAdded |
	buttonRemovePackage
		action: [ 
			self selectedItemsFromPackageChosenList
				ifNotEmpty: [ 
					itemsAdded := self itemsFromPackageChosenList.
					self selectedItemsFromPackageChosenList do: [ :each | itemsAdded remove: each ].
					self packageChosenList items: itemsAdded.
					self packageChosenList resetSelection.
					self updatePackageChosenLabel ] ]
]

{ #category : #actions }
DAWelcomePresenter >> buildList [
	self packageList items: self systemPackages.
	self updateLabels
]

{ #category : #accessing }
DAWelcomePresenter >> buttonAddAll [
	^ buttonAddAll
]

{ #category : #accessing }
DAWelcomePresenter >> buttonAddPackage [
	^ buttonAddPackage
]

{ #category : #accessing }
DAWelcomePresenter >> buttonOpen [
	^ buttonOpen
]

{ #category : #accessing }
DAWelcomePresenter >> buttonRemoveAll [
	^ buttonRemoveAll
]

{ #category : #accessing }
DAWelcomePresenter >> buttonRemovePackage [
	^ buttonRemovePackage
]

{ #category : #initialization }
DAWelcomePresenter >> filterPackageList [
	| match itemsMatching |
	textPackageField text
		ifEmpty: [ self packageList items: self systemPackages ]
		ifNotEmpty: [ 
			match := '*' , textPackageField text , '*'.
			itemsMatching := self systemPackages select: [ :package | match match: package ].
			self packageList items: itemsMatching asSortedCollection ]
]

{ #category : #'morphic ui' }
DAWelcomePresenter >> initialExtent [
	^ (800@800)
]

{ #category : #initialization }
DAWelcomePresenter >> initializeButtons [
	buttonOpen := self newButton
		help: 'Open the package dependencies analysis';
		label: 'Browse dependencies';
		icon: (self iconNamed: #packageIcon).
	buttonAddPackage := self newButton
		help: 'Add package to browse';
		label: 'Add package';
		icon: (self iconNamed: #addIcon).
	buttonAddAll := self newButton
		help: 'Add all packages';
		label: 'Add all';
		icon: (self iconNamed: #addIcon).
	buttonRemovePackage := self newButton
		help: 'Remove package to browse';
		label: 'Remove package';
		icon: (self iconNamed: #deleteIcon).
	buttonRemoveAll := self newButton
		help: 'Remove all selected packages';
		label: 'Remove all';
		icon: (self iconNamed: #deleteIcon)
]

{ #category : #initialization }
DAWelcomePresenter >> initializeLabels [
	packageLabel := self newLabel.
	packageChosenLabel := self newLabel
]

{ #category : #initialization }
DAWelcomePresenter >> initializeLists [
	packageList := self newList
		multiSelection: true;
		yourself.
	packageChosenList := self newList
		multiSelection: true;
		yourself
]

{ #category : #initialization }
DAWelcomePresenter >> initializePresenter [
	self actionOnOpen;
		actionOnAddPackage;
		actionOnRemovePackage.
	
	self buttonAddAll action: [ self packageChosenList items: self packageList listItems. self updatePackageChosenLabel ].
	self buttonRemoveAll action: [ self packageChosenList items: #().  self updatePackageChosenLabel ].
		
	packageChosenList whenSelectedItemChanged: [ packageList resetSelection ].
	
	textPackageField
		whenTextChanged: [ self filterPackageList ]
]

{ #category : #initialization }
DAWelcomePresenter >> initializeWidgets [
	self
		initializeLists;
		initializeLabels;
		initializeButtons;
		buildList.
		
	textPackageField := self newTextInput
		ghostText: 'Filter package by name';
		entryCompletion: self packagesEntryCompletion.
]

{ #category : #accessing }
DAWelcomePresenter >> itemsFromPackageChosenList [
	^ self packageChosenList listItems
]

{ #category : #merging }
DAWelcomePresenter >> merge: aCollection With: anOtherCollection [
	^ aCollection asSet
		addAll: anOtherCollection;
		yourself
]

{ #category : #actions }
DAWelcomePresenter >> open [
	^ self openWithSpec
]

{ #category : #accessing }
DAWelcomePresenter >> packageChosenLabel [
	^ packageChosenLabel
]

{ #category : #accessing }
DAWelcomePresenter >> packageChosenList [
	^ packageChosenList
]

{ #category : #accessing }
DAWelcomePresenter >> packageLabel [
	^ packageLabel
]

{ #category : #accessing }
DAWelcomePresenter >> packageList [
	^ packageList
]

{ #category : #accessing }
DAWelcomePresenter >> packagesEntryCompletion [
	| applicants |
	applicants := self systemPackages.

	^ EntryCompletion new
				dataSourceBlock: [:currText | applicants];
				filterBlock: [:currApplicant :currText | currText size > 3
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].
]

{ #category : #accessing }
DAWelcomePresenter >> selectedItemsFromPackageChosenList [
	^ self packageChosenList selectedItems
]

{ #category : #accessing }
DAWelcomePresenter >> selectedItemsFromPackageList [
	^ self packageList selectedItems
]

{ #category : #accessing }
DAWelcomePresenter >> systemPackages [
	^ (RPackageOrganizer default packages collect: [ :package | package packageName asString ] )asSortedCollection
]

{ #category : #accessing }
DAWelcomePresenter >> textPackageField [
	^ textPackageField
]

{ #category : #'morphic ui' }
DAWelcomePresenter >> title [
	^ 'Package Dependencies Browser'
]

{ #category : #protocol }
DAWelcomePresenter >> updateLabels [
	self
		updatePackageLabel;
		updatePackageChosenLabel
]

{ #category : #protocol }
DAWelcomePresenter >> updatePackageChosenLabel [
	self packageChosenLabel 
			label: packageChosenList listSize asString, ' package(s) chosen'.
]

{ #category : #protocol }
DAWelcomePresenter >> updatePackageLabel [
	packageLabel 
			label: 'Choose your packages (', self packageList listSize asString, ' packages available)'.
]
