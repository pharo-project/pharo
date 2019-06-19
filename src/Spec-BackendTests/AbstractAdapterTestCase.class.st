Class {
	#name : #AbstractAdapterTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'presenter',
		'specInitializationStrategy',
		'window',
		'backendForTest'
	],
	#category : #'Spec-BackendTests'
}

{ #category : #testing }
AbstractAdapterTestCase class >> isAbstract [ 
	^ self == AbstractAdapterTestCase
]

{ #category : #testing }
AbstractAdapterTestCase class >> testParameters [ 

	^ ParametrizedTestMatrix new
		forSelector: #specInitializationStrategy
			addOptions: { [ SpecInitializationStrategy beforeTest ]. [ SpecInitializationStrategy afterTest ] };
		forSelector: #backendForTest
			addOptions: AbstractBackendForTest allSubclasses;
		yourself
]

{ #category : #running }
AbstractAdapterTestCase >> adapter [

	"Force opening the spec instance here.
	The action should have been correctly configured before
	depending on the spec initialization strategy"
	backendForTest openInstanceOf: self.
	^ presenter adapter
]

{ #category : #asserting }
AbstractAdapterTestCase >> assertIcon: anIcon equals: aForm [ 
	
	backendForTest assertIcon: anIcon equals: aForm
]

{ #category : #accessing }
AbstractAdapterTestCase >> backendForTest: aClass [ 
	
	backendForTest := aClass new
]

{ #category : #accessing }
AbstractAdapterTestCase >> classToTest [

	^ self subclassResponsibility
]

{ #category : #utils }
AbstractAdapterTestCase >> emulateCenterClick [

	self emulateClick: MouseButtonEvent blueButton
]

{ #category : #utils }
AbstractAdapterTestCase >> emulateClick: mouseButtonCode [

	backendForTest emulateClick: mouseButtonCode onTest: self.
]

{ #category : #utils }
AbstractAdapterTestCase >> emulateLeftClick [

	self emulateClick: MouseButtonEvent redButton
]

{ #category : #utils }
AbstractAdapterTestCase >> emulateRightClick [

	self emulateClick: MouseButtonEvent yellowButton
]

{ #category : #running }
AbstractAdapterTestCase >> initializeTestedInstance [
]

{ #category : #running }
AbstractAdapterTestCase >> openInstance [

	backendForTest openInstanceOf: self 

]

{ #category : #private }
AbstractAdapterTestCase >> performTest [
	
	specInitializationStrategy beforeTest: self.
	super performTest.
]

{ #category : #running }
AbstractAdapterTestCase >> prepareToRunAgain [

	backendForTest runTest: [ super prepareToRunAgain ]

]

{ #category : #accessing }
AbstractAdapterTestCase >> presenter [
	^ presenter
]

{ #category : #running }
AbstractAdapterTestCase >> runCase [

	self resources do: [:each | each availableFor: self].
	[	super setUp.
		backendForTest runTest: [
			presenter := self classToTest new.
			self initializeTestedInstance.
			self performTest].
	] ensure: [
		self tearDown.
		self cleanUpInstanceVariables]
]

{ #category : #running }
AbstractAdapterTestCase >> setUp [
	super setUp.	"This super is useless but it's to make the release tests happy."
	self error: 'Do not override me.... use #initializeTestedInstance'
]

{ #category : #accessing }
AbstractAdapterTestCase >> specInitializationStrategy: aStrategy [
	
	specInitializationStrategy := aStrategy
]

{ #category : #running }
AbstractAdapterTestCase >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #running }
AbstractAdapterTestCase >> testHidePresenterHidesWidget [
	
	self presenter hide.
	self deny: self adapter isVisible
]

{ #category : #running }
AbstractAdapterTestCase >> testShowPresenterShowsWidget [
	
	self presenter show.
	self assert: self adapter isVisible
]

{ #category : #running }
AbstractAdapterTestCase >> testVisibilityWithBlock [
	presenter visibleIf: [ true ].
	self assert: self adapter isVisible
]

{ #category : #running }
AbstractAdapterTestCase >> testVisibilityWithBlock2 [
	presenter visibleIf: [ false ].
	self deny: self adapter isVisible
]

{ #category : #running }
AbstractAdapterTestCase >> widget [

	^ self adapter widget
]

{ #category : #accessing }
AbstractAdapterTestCase >> window [
	^ window
]

{ #category : #accessing }
AbstractAdapterTestCase >> window: aWindowPresenter [ 
	window := aWindowPresenter
]
