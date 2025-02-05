"
I present dependency information on a system package.
"
Class {
	#name : 'DATabPackageDependenciesPresenter',
	#superclass : 'SpPresenterWithModel',
	#instVars : [
		'packageList',
		'dependencyList',
		'browser',
		'openButton'
	],
	#category : 'Tool-DependencyAnalyser-UI-Tab',
	#package : 'Tool-DependencyAnalyser-UI-Tab'
}

{ #category : 'accessing' }
DATabPackageDependenciesPresenter class >> defaultPreferredExtent [ 

	^ 800 @ 400
]

{ #category : 'examples' }
DATabPackageDependenciesPresenter class >> example [

	| analyzedPackage |
	analyzedPackage := DADependencyChecker new daPackageFor: 'Tool-Diff'.
	(self on: analyzedPackage) open
]

{ #category : 'accessing' }
DATabPackageDependenciesPresenter >> analyzedPackage [

	^ self model
]

{ #category : 'initialization' }
DATabPackageDependenciesPresenter >> analyzedPackage: aDAPackage [

	self model: aDAPackage
]

{ #category : 'initialization' }
DATabPackageDependenciesPresenter >> browse: aDependency [

	browser ifNil: [ ^ self ].
	aDependency ifNil: [ ^ self ].

	aDependency browseIn: browser
]

{ #category : 'accessing' }
DATabPackageDependenciesPresenter >> browser: aSystemBrowser [

	browser := aSystemBrowser
]

{ #category : 'initialization' }
DATabPackageDependenciesPresenter >> connectPresenters [

	packageList
		transmitTo: dependencyList
		transform: [ :package | package ifNil: [ #() ] ifNotNil: [ package dependencies ] ]
]

{ #category : 'layout' }
DATabPackageDependenciesPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		spacing: 5;
		add: (SpBoxLayout newLeftToRight
			add: ('Dependencies of {1}:' format: { self analyzedPackage packageName}) expand: false;
			addLast: openButton) expand: false;
		add: (SpBoxLayout newLeftToRight
			spacing: 10;
			add: packageList;
			add: dependencyList;
			yourself);
		yourself
]

{ #category : 'initialization' }
DATabPackageDependenciesPresenter >> initializePresenters [

	packageList := self newList
		items: self packageDependencies;
		display: [ :daPackage | daPackage packageName, ' ({1})' format: { daPackage dependencies size } ];
		displayIcon: [ :item | self iconNamed: #package ];
		yourself.

	dependencyList := self newList
		displayIcon: [ :item | item nodeClass new icon ];
		whenSelectionChangedDo: [ :selection | self browse: selection selectedItem ];
		yourself.

	openButton := self newButton
		label: 'Open Dependency Analyzer';
		icon: (self iconNamed: #objects);
		action: [ self openDependencyAnalyzer ];
		yourself
]

{ #category : 'initialization' }
DATabPackageDependenciesPresenter >> modelChanged [

	packageList items: self packageDependencies
]

{ #category : 'actions' }
DATabPackageDependenciesPresenter >> openDependencyAnalyzer [

	(DAPackageDependenciesPresenter
			onPackagesNamed: {self analyzedPackage packageName}) open
]

{ #category : 'accessing' }
DATabPackageDependenciesPresenter >> packageDependencies [

	^ ((self analyzedPackage dependencies)
		sorted: [ :a :b | a target packageName < b target packageName ])
]
