"
Welcome user interface to browse dependencies packages. Availaible via tools menu.
"
Class {
	#name : #DAPackageDependenciesWelcome,
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
DAPackageDependenciesWelcome class >> defaultSpec [
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
DAPackageDependenciesWelcome class >> menuCommandOn: aBuilder [
	<worldMenu>

	(aBuilder item: #PackageDependencies)
		order: 0.21;
		parent: #Tools;
		label: 'Dependency Analyser';
		icon: (self iconNamed: #packageIcon);
		help: 'Analyze dependencies between different packages in the image.';
		action: [ self new open ]
]

{ #category : #protocol }
DAPackageDependenciesWelcome >> actionOnAddPackage [
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
DAPackageDependenciesWelcome >> actionOnOpen [
	buttonOpen
		action: [ 
			self itemsFromPackageChosenList
				ifNotEmpty: [ 
					(DAPackageAnalyzerPackageDependenciesWindow onPackagesNamed: self itemsFromPackageChosenList) open.
					 ] ]
]

{ #category : #protocol }
DAPackageDependenciesWelcome >> actionOnRemovePackage [
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
DAPackageDependenciesWelcome >> buildList [
	self packageList items: self systemPackages.
	self updateLabels
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> buttonAddAll [
	^ buttonAddAll
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> buttonAddPackage [
	^ buttonAddPackage
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> buttonOpen [
	^ buttonOpen
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> buttonRemoveAll [
	^ buttonRemoveAll
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> buttonRemovePackage [
	^ buttonRemovePackage
]

{ #category : #initialization }
DAPackageDependenciesWelcome >> filterPackageList [
	| match itemsMatching |
	textPackageField text
		ifEmpty: [ self packageList items: self systemPackages ]
		ifNotEmpty: [ 
			match := '*' , textPackageField text , '*'.
			itemsMatching := self systemPackages select: [ :package | match match: package ].
			self packageList items: itemsMatching asSortedCollection ]
]

{ #category : #'morphic ui' }
DAPackageDependenciesWelcome >> initialExtent [
	^ (800@800)
]

{ #category : #initialization }
DAPackageDependenciesWelcome >> initializeButtons [
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
		icon: (self icon: #deleteIcon).
	buttonRemoveAll := self newButton
		help: 'Remove all selected packages';
		label: 'Remove all';
		icon: (self icon: #deleteIcon)
]

{ #category : #initialization }
DAPackageDependenciesWelcome >> initializeLabels [
	packageLabel := self newLabel.
	packageChosenLabel := self newLabel
]

{ #category : #initialization }
DAPackageDependenciesWelcome >> initializeLists [
	packageList := self newList
		multiSelection: true;
		yourself.
	packageChosenList := self newList
		multiSelection: true;
		yourself
]

{ #category : #initialization }
DAPackageDependenciesWelcome >> initializePresenter [
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
DAPackageDependenciesWelcome >> initializeWidgets [
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
DAPackageDependenciesWelcome >> itemsFromPackageChosenList [
	^ self packageChosenList listItems
]

{ #category : #merging }
DAPackageDependenciesWelcome >> merge: aCollection With: anOtherCollection [
	^ aCollection asSet
		addAll: anOtherCollection;
		yourself
]

{ #category : #actions }
DAPackageDependenciesWelcome >> open [
	^ self openWithSpec
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> packageChosenLabel [
	^ packageChosenLabel
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> packageChosenList [
	^ packageChosenList
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> packageLabel [
	^ packageLabel
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> packageList [
	^ packageList
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> packagesEntryCompletion [
	| applicants |
	applicants := self systemPackages.

	^ EntryCompletion new
				dataSourceBlock: [:currText | applicants];
				filterBlock: [:currApplicant :currText | currText size > 3
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> selectedItemsFromPackageChosenList [
	^ self packageChosenList selectedItems
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> selectedItemsFromPackageList [
	^ self packageList selectedItems
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> systemPackages [
	^ (RPackageOrganizer default packages collect: [ :package | package packageName asString ] )asSortedCollection
]

{ #category : #accessing }
DAPackageDependenciesWelcome >> textPackageField [
	^ textPackageField
]

{ #category : #'morphic ui' }
DAPackageDependenciesWelcome >> title [
	^ 'Package Dependencies Browser'
]

{ #category : #protocol }
DAPackageDependenciesWelcome >> updateLabels [
	self
		updatePackageLabel;
		updatePackageChosenLabel
]

{ #category : #protocol }
DAPackageDependenciesWelcome >> updatePackageChosenLabel [
	self packageChosenLabel 
			label: packageChosenList listSize asString, ' package(s) chosen'.
]

{ #category : #protocol }
DAPackageDependenciesWelcome >> updatePackageLabel [
	self packageLabel 
			label: 'Choose your packages (', self packageList listSize asString, ' packages available)'.
]
