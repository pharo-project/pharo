"
I present dependency information on a system package.
"
Class {
	#name : #DATabPackageDependenciesPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'packageList',
		'analyzedPackage',
		'dependencyList'
	],
	#category : #'Tool-DependencyAnalyser-UI-Tab'
}

{ #category : #examples }
DATabPackageDependenciesPresenter class >> example [

	| analyzedPackage |
	analyzedPackage := DADependencyChecker new daPackageFor: 'Tool-Diff'.
	(self on: analyzedPackage) open
]

{ #category : #initialization }
DATabPackageDependenciesPresenter >> connectPresenters [ 

	packageList transmitTo: dependencyList transform: [ :package | package dependencies ]
]

{ #category : #layout }
DATabPackageDependenciesPresenter >> defaultLayout [ 

	^ SpBoxLayout newTopToBottom 
		spacing: 5;
		add: ('Dependencies of {1}:' format: { analyzedPackage packageName}) expand: false;
		add: (SpBoxLayout newLeftToRight
			spacing: 10;
			add: packageList;
			add: dependencyList;
			yourself);
		yourself
]

{ #category : #initialization }
DATabPackageDependenciesPresenter >> initializePresenters [

	packageList := self newList
		items: self packageDependencies;
		display: [ :daPackage | daPackage packageName, ' ({1})' format: { daPackage dependencies size } ];
		displayIcon: [ :item | self iconNamed: #package ];
		yourself.
		
	dependencyList := self newList
		displayIcon: [ :item | item nodeClass new icon ];
		yourself.
]

{ #category : #initialization }
DATabPackageDependenciesPresenter >> initializeWindow: aWindowPresenter [
	
	aWindowPresenter initialExtent: 800 @ 400
]

{ #category : #accessing }
DATabPackageDependenciesPresenter >> packageDependencies [
	
	^ ((analyzedPackage dependencies) 
		sorted: [ :a :b | a target packageName < b target packageName ])
]

{ #category : #initialization }
DATabPackageDependenciesPresenter >> setModelBeforeInitialization: aDAPackage [

	analyzedPackage := aDAPackage
]
