"
A simple CompasableModel to add new packages to the current package dependencies analysis.
It implies to be open from a PDPackageAnalyzerWindowPackageDependencies instance. 
"
Class {
	#name : #DAAddPackagePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'packageList',
		'buttonAddPackage',
		'packageAnalyzerTreeModel'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #specs }
DAAddPackagePresenter class >> defaultSpec [
	<spec: #default>
	^ SpBoxLayout newTopToBottom
			add: #packageList;
			add: #buttonAddPackage expand: false;
			yourself
]

{ #category : #protocol }
DAAddPackagePresenter >> actionOnAddPackage [
	| relationGraph packagesToAdd |
	buttonAddPackage
		action: [ 
			self selectedItemsFromPackageList
				ifNotEmpty: [ 
					relationGraph := self packageAnalyzerTreeModel relationGraph.
					packagesToAdd := self selectedItemsFromPackageList
						collect: [ :packageName | (DAPackage on: (RPackageSet named: packageName)) beSeen ].
					relationGraph addPackages: packagesToAdd.
					self packageAnalyzerTreeModel
						refresh;
						resetDefaultSettings.
					self delete ] ]
]

{ #category : #actions }
DAAddPackagePresenter >> buildList [
	| packagesAdded packagesNotAdded |
	packagesAdded := self packageAnalyzerTreeModel seenPackagesName.
	packagesNotAdded := self systemPackages.
	packagesNotAdded removeAll: packagesAdded.
	packageList sourceItems: packagesNotAdded.
]

{ #category : #accessing }
DAAddPackagePresenter >> buttonAddPackage [
	^ buttonAddPackage
]

{ #category : #initialization }
DAAddPackagePresenter >> connectPresenters [
	self actionOnAddPackage
]

{ #category : #api }
DAAddPackagePresenter >> initialExtent [
	^ (600@600)
]

{ #category : #initialization }
DAAddPackagePresenter >> initializePresenters [
	
	buttonAddPackage := self newButton
		help: 'Add these packages to the current analysis';
		label: 'Add packages';
		icon: (self iconNamed: #addIcon);
		yourself.

	packageList := SpChooserPresenter new.
]

{ #category : #'instance creation' }
DAAddPackagePresenter >> open [
	^ self openWithSpec
]

{ #category : #accessing }
DAAddPackagePresenter >> packageAnalyzerTreeModel [
	^ packageAnalyzerTreeModel
]

{ #category : #accessing }
DAAddPackagePresenter >> packageAnalyzerTreeModel: aPackageAnalyzerTreeModel [
	packageAnalyzerTreeModel := aPackageAnalyzerTreeModel.
	self buildList
]

{ #category : #accessing }
DAAddPackagePresenter >> packageList [
	^ packageList
]

{ #category : #initialization }
DAAddPackagePresenter >> selectedItemsFromPackageList [
	^ self packageList chosenItems
]

{ #category : #accessing }
DAAddPackagePresenter >> systemPackages [
	^ (RPackageOrganizer default packages collect: [ :package | package packageName asString ] )asSortedCollection
]

{ #category : #protocol }
DAAddPackagePresenter >> title [
	^ 'Choose packages to add'
]
