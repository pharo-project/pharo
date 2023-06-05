"
Welcome user interface to browse dependencies packages. Availaible via tools menu.

self new openWithSpec
"
Class {
	#name : #DAWelcomePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'choosePresenter',
		'browseDependenciesButton'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #'world menu' }
DAWelcomePresenter class >> menuCommandOn: aBuilder [
	<worldMenu>

	(aBuilder item: #PackageDependencies)
		order: 2;
		parent: #Packaging;
		label: 'Dependency Analyser';
		icon: (self iconNamed: #package);
		help: 'Analyze dependencies between different packages in the image.';
		action: [ self new open ]
]

{ #category : #layout }
DAWelcomePresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		add: choosePresenter;
		add: browseDependenciesButton expand: false;
		yourself
]

{ #category : #accessing }
DAWelcomePresenter >> initialExtent [
	^ (800@800)
]

{ #category : #initialization }
DAWelcomePresenter >> initializePresenters [

	choosePresenter := SpChooserPresenter new
		sourceItems: Smalltalk organization packages;
		displayBlock: #packageName;
		yourself.

	browseDependenciesButton := self newButton
		help: 'Open the package dependencies analysis';
		label: 'Browse dependencies';
		icon: (self iconNamed: #package);
		action: [ self selectedPackages ifNotEmpty: [ self openDependencyAnalyzer ] ]
		yourself
]

{ #category : #initialization }
DAWelcomePresenter >> initializeWindow: 	aWindowPresenter [
	aWindowPresenter
		title: self title;
		initialExtent: self initialExtent
]

{ #category : #action }
DAWelcomePresenter >> openDependencyAnalyzer [
	(DAPackageDependenciesPresenter onPackagesNamed: self selectedPackageNames) open.
	choosePresenter delete
]

{ #category : #accessing }
DAWelcomePresenter >> selectedPackageNames [
	^ self selectedPackages collect: [ :each | each name ]
]

{ #category : #accessing }
DAWelcomePresenter >> selectedPackages [
	^ choosePresenter chosenItems
]

{ #category : #accessing }
DAWelcomePresenter >> title [
	^ 'Package Dependencies Browser'
]
