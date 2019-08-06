Class {
	#name : #SpAbstractAdapterTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'presenter',
		'specInitializationStrategy',
		'window',
		'backendForTest'
	],
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractAdapterTestCase class >> isAbstract [ 
	^ self == SpAbstractAdapterTestCase
]

{ #category : #testing }
SpAbstractAdapterTestCase class >> testParameters [ 

	^ ParametrizedTestMatrix new
		forSelector: #specInitializationStrategy
			addOptions: { [ SpInitializationStrategy beforeTest ]. [ SpInitializationStrategy afterTest ] };
		forSelector: #backendForTest
			addOptions: SpAbstractBackendForTest allSubclasses;
		yourself
]

{ #category : #running }
SpAbstractAdapterTestCase >> adapter [

	"Force opening the spec instance here.
	The action should have been correctly configured before
	depending on the spec initialization strategy"
	backendForTest openInstanceOf: self.
	^ presenter adapter
]

{ #category : #asserting }
SpAbstractAdapterTestCase >> assertIcon: anIcon equals: aForm [ 
	
	backendForTest assertIcon: anIcon equals: aForm
]

{ #category : #accessing }
SpAbstractAdapterTestCase >> backendForTest: aClass [ 
	
	backendForTest := aClass new
]

{ #category : #accessing }
SpAbstractAdapterTestCase >> classToTest [

	^ self subclassResponsibility
]

{ #category : #utils }
SpAbstractAdapterTestCase >> emulateCenterClick [

	self emulateClick: MouseButtonEvent blueButton
]

{ #category : #utils }
SpAbstractAdapterTestCase >> emulateClick: mouseButtonCode [

	backendForTest emulateClick: mouseButtonCode onTest: self.
]

{ #category : #utils }
SpAbstractAdapterTestCase >> emulateLeftClick [

	self emulateClick: MouseButtonEvent redButton
]

{ #category : #utils }
SpAbstractAdapterTestCase >> emulateRightClick [

	self emulateClick: MouseButtonEvent yellowButton
]

{ #category : #running }
SpAbstractAdapterTestCase >> initializeTestedInstance [
]

{ #category : #running }
SpAbstractAdapterTestCase >> openInstance [

	backendForTest openInstanceOf: self 

]

{ #category : #running }
SpAbstractAdapterTestCase >> performTest [

	backendForTest runTest: [
		presenter := self classToTest new.
		self initializeTestedInstance.
		super performTest]
]

{ #category : #running }
SpAbstractAdapterTestCase >> prepareToRunAgain [ 	

	"Redefined because setup is cancelled in this class"
	self tearDown.
	super setUp.
]

{ #category : #accessing }
SpAbstractAdapterTestCase >> presenter [
	^ presenter
]

{ #category : #running }
SpAbstractAdapterTestCase >> runCase [
	self resources do: [:each | each availableFor: self].
	[super setUp.
	self performTest] ensure: [
		self tearDown.
		self cleanUpInstanceVariables]
]

{ #category : #running }
SpAbstractAdapterTestCase >> setUp [
	super setUp.	"This super is useless but it's to make the release tests happy."
	self error: 'Do not override me.... use #initializeTestedInstance'
]

{ #category : #accessing }
SpAbstractAdapterTestCase >> specInitializationStrategy: aStrategy [
	
	specInitializationStrategy := aStrategy
]

{ #category : #running }
SpAbstractAdapterTestCase >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #running }
SpAbstractAdapterTestCase >> testHidePresenterHidesWidget [
	
	self presenter hide.
	self deny: self adapter isVisible
]

{ #category : #running }
SpAbstractAdapterTestCase >> testShowPresenterShowsWidget [
	
	self presenter show.
	self assert: self adapter isVisible
]

{ #category : #running }
SpAbstractAdapterTestCase >> testVisibilityWithBlock [
	presenter visibleIf: [ true ].
	self assert: self adapter isVisible
]

{ #category : #running }
SpAbstractAdapterTestCase >> testVisibilityWithBlock2 [
	presenter visibleIf: [ false ].
	self deny: self adapter isVisible
]

{ #category : #running }
SpAbstractAdapterTestCase >> widget [

	^ self adapter widget
]

{ #category : #accessing }
SpAbstractAdapterTestCase >> window [
	^ window
]

{ #category : #accessing }
SpAbstractAdapterTestCase >> window: aWindowPresenter [ 
	window := aWindowPresenter
]
