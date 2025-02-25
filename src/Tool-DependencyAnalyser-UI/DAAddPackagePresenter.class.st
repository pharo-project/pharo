"
A simple CompasableModel to add new packages to the current package dependencies analysis.
It implies to be open from a PDPackageAnalyzerWindowPackageDependencies instance. 
"
Class {
	#name : 'DAAddPackagePresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'packageList',
		'buttonAddPackage',
		'dependenciesPresenter'
	],
	#category : 'Tool-DependencyAnalyser-UI-Core',
	#package : 'Tool-DependencyAnalyser-UI',
	#tag : 'Core'
}

{ #category : 'specs' }
DAAddPackagePresenter class >> defaultPreferredExtent [
	^ (600@600)
]

{ #category : 'specs' }
DAAddPackagePresenter class >> title [

	^ 'Choose packages to add'
]

{ #category : 'protocol' }
DAAddPackagePresenter >> actionOnAddPackage [

	| relationGraph packagesToAdd |
	buttonAddPackage action: [
		self selectedItemsFromPackageList ifNotEmpty: [
			relationGraph := self dependenciesPresenter relationGraph.
			packagesToAdd := self selectedItemsFromPackageList collect: [ :packageName | (DAPackage onPackageNamed: packageName) beSeen ].
			relationGraph addPackages: packagesToAdd.
			self dependenciesPresenter
				refresh;
				resetDefaultSettings.
			self delete ] ]
]

{ #category : 'actions' }
DAAddPackagePresenter >> buildList [
	| packagesAdded packagesNotAdded |
	packagesAdded := self dependenciesPresenter seenPackagesName.
	packagesNotAdded := self systemPackages.
	packagesNotAdded removeAll: packagesAdded.
	packageList sourceItems: packagesNotAdded
]

{ #category : 'initialization' }
DAAddPackagePresenter >> connectPresenters [
	self actionOnAddPackage
]

{ #category : 'layout' }
DAAddPackagePresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
			add: packageList;
			add: buttonAddPackage expand: false;
			yourself
]

{ #category : 'accessing' }
DAAddPackagePresenter >> dependenciesPresenter [
	^ dependenciesPresenter
]

{ #category : 'accessing' }
DAAddPackagePresenter >> dependenciesPresenter: aPackageTreePresenter [
	dependenciesPresenter := aPackageTreePresenter.
	self buildList
]

{ #category : 'initialization' }
DAAddPackagePresenter >> initializePresenters [

	buttonAddPackage := self newButton
		help: 'Add these packages to the current analysis';
		label: 'Add packages';
		icon: (self iconNamed: #add);
		yourself.

	packageList := SpChooserPresenter new
]

{ #category : 'initialization' }
DAAddPackagePresenter >> selectedItemsFromPackageList [
	^ packageList chosenItems
]

{ #category : 'accessing' }
DAAddPackagePresenter >> systemPackages [

	^ (self packageOrganizer packages collect: [ :package | package name asString ]) asSortedCollection
]
