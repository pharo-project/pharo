"
A simple CompasableModel to add new packages to the current package dependencies analysis.
It implies to be open from a PDPackageAnalyzerWindowPackageDependencies instance. 
"
Class {
	#name : #DAAddPackagePresenter,
	#superclass : #ComposablePresenter,
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
	^ SpecLayout composed
		newColumn:
			[ :col | 
			col
				add: #packageList;
				add: #buttonAddPackage height: 30
				] yourself
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
	packageList items: packagesNotAdded.
]

{ #category : #accessing }
DAAddPackagePresenter >> buttonAddPackage [
	^ buttonAddPackage
]

{ #category : #api }
DAAddPackagePresenter >> initialExtent [
	^ (600@600)
]

{ #category : #initialization }
DAAddPackagePresenter >> initializePresenter [
	self actionOnAddPackage
]

{ #category : #initialization }
DAAddPackagePresenter >> initializeWidgets [
	
	packageList := self newList
		multiSelection: true;
		yourself.
	
	buttonAddPackage := self newButton
		help: 'Add these packages to the current analysis';
		label: 'Add packages';
		icon: (self iconNamed: #addIcon);
		yourself.
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
	^ self packageList selectedItems
]

{ #category : #accessing }
DAAddPackagePresenter >> systemPackages [
	^ (RPackageOrganizer default packages collect: [ :package | package packageName asString ] )asSortedCollection
]

{ #category : #protocol }
DAAddPackagePresenter >> title [
	^ 'Choose packages to add'
]
