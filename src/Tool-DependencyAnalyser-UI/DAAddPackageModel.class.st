"
A simple CompasableModel to add new packages to the current package dependencies analysis.
It implies to be open from a PDPackageAnalyzerWindowPackageDependencies instance. 
"
Class {
	#name : #DAAddPackageModel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'packageList',
		'buttonAddPackage',
		'packageAnalyzerTreeModel'
	],
	#category : 'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #specs }
DAAddPackageModel class >> defaultSpec [
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
DAAddPackageModel >> actionOnAddPackage [
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
DAAddPackageModel >> buildList [
	| packagesAdded packagesNotAdded |
	packagesAdded := self packageAnalyzerTreeModel seenPackagesName.
	packagesNotAdded := self systemPackages.
	packagesNotAdded removeAll: packagesAdded.
	packageList items: packagesNotAdded.
]

{ #category : #accessing }
DAAddPackageModel >> buttonAddPackage [
	^ buttonAddPackage
]

{ #category : #api }
DAAddPackageModel >> initialExtent [
	^ (600@600)
]

{ #category : #initialization }
DAAddPackageModel >> initializePresenter [
	self actionOnAddPackage
]

{ #category : #initialization }
DAAddPackageModel >> initializeWidgets [
	
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
DAAddPackageModel >> open [
	^ self openWithSpec
]

{ #category : #accessing }
DAAddPackageModel >> packageAnalyzerTreeModel [
	^ packageAnalyzerTreeModel
]

{ #category : #accessing }
DAAddPackageModel >> packageAnalyzerTreeModel: aPackageAnalyzerTreeModel [
	packageAnalyzerTreeModel := aPackageAnalyzerTreeModel.
	self buildList
]

{ #category : #accessing }
DAAddPackageModel >> packageList [
	^ packageList
]

{ #category : #initialization }
DAAddPackageModel >> selectedItemsFromPackageList [
	^ self packageList selectedItems
]

{ #category : #accessing }
DAAddPackageModel >> systemPackages [
	^ (RPackageOrganizer default packages collect: [ :package | package packageName asString ] )asSortedCollection
]

{ #category : #protocol }
DAAddPackageModel >> title [
	^ 'Choose packages to add'
]
