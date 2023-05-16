"
A DAPackageAnalyzerWindow class is an abstract presenter.
It defines a Spec Tree (package dependencies or cycles) as well as a system browser on the right side.

The main feature is to browse automatically in the browser when you click on a node (package or dependency).

"
Class {
	#name : #DAPackageAnalyzerPresenter,
	#superclass : #SpPresenterWithModel,
	#instVars : [
		'browserWrapper'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #testing }
DAPackageAnalyzerPresenter class >> isAbstract [

	^ self == DAPackageAnalyzerPresenter
]

{ #category : #'instance creation' }
DAPackageAnalyzerPresenter class >> onPackagesNamed: aCollection [
	^ self on: (DADependencyTreePresenter onPackagesNamed: aCollection)
]

{ #category : #accessing }
DAPackageAnalyzerPresenter class >> selectedPackagesFrom: aBuilder [
	| target selectedPackages |
	target := aBuilder model.
	selectedPackages := target selectedPackages collect: [ :rpackage | rpackage packageName asString ].
	^ selectedPackages
]

{ #category : #examples }
DAPackageAnalyzerPresenter class >> systemPackages [
	^ Smalltalk organization packages collect: [ :package | package packageName asString ]
]

{ #category : #accessing }
DAPackageAnalyzerPresenter >> browserWindow [

	^ self model browser
]

{ #category : #'morphic ui' }
DAPackageAnalyzerPresenter >> defaultLabel [
	^ 'Package Dependencies Analysis'
]

{ #category : #layout }
DAPackageAnalyzerPresenter >> defaultLayout [
	^ SpPanedLayout newHorizontal
		add: self model;
		add: browserWrapper;
		positionOfSlider: 0.3;
		yourself
]

{ #category : #'open/close' }
DAPackageAnalyzerPresenter >> delete [
	self dependenciesPresenter close.
	^ super delete
]

{ #category : #accessing }
DAPackageAnalyzerPresenter >> dependenciesPresenter [
	^ self model
]

{ #category : #'morphic ui' }
DAPackageAnalyzerPresenter >> initialExtent [
	^ (1500@800)
]

{ #category : #initialization }
DAPackageAnalyzerPresenter >> initializePresenters [

	browserWrapper := self newMorph
		morph: self browserWindow
]
