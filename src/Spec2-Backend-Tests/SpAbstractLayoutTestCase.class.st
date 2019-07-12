Class {
	#name : #SpAbstractLayoutTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'presenter',
		'window',
		'backendForTest'
	],
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractLayoutTestCase class >> isAbstract [ 
	^ self = SpAbstractLayoutTestCase
]

{ #category : #testing }
SpAbstractLayoutTestCase class >> testParameters [
	^ ParametrizedTestMatrix new
		forSelector: #backendForTest addOptions: SpAbstractBackendForTest allSubclasses;
		yourself
]

{ #category : #running }
SpAbstractLayoutTestCase >> adapter [

	"Force opening the spec instance here.
	The action should have been correctly configured before
	depending on the spec initialization strategy"
	backendForTest openInstanceOf: self.
	^ presenter adapter
]

{ #category : #accessing }
SpAbstractLayoutTestCase >> backendForTest: aClass [ 
	
	backendForTest := aClass new
]

{ #category : #accessing }
SpAbstractLayoutTestCase >> classToTest [

	^ self subclassResponsibility
]

{ #category : #running }
SpAbstractLayoutTestCase >> openInstance [

	backendForTest openInstanceOf: self 

]

{ #category : #running }
SpAbstractLayoutTestCase >> prepareToRunAgain [

	backendForTest runTest: [ super prepareToRunAgain ]

]

{ #category : #accessing }
SpAbstractLayoutTestCase >> presenter [
	^ presenter
]

{ #category : #running }
SpAbstractLayoutTestCase >> runCase [

	self resources do: [:each | each availableFor: self].
	[	super setUp.
		backendForTest runTest: [
			presenter := self classToTest new.
			self performTest].
	] ensure: [
		self tearDown.
		self cleanUpInstanceVariables]
]

{ #category : #running }
SpAbstractLayoutTestCase >> setUp [
	super setUp.	"This super is useless but it's to make the release tests happy."
	self error: 'Do not override me.... use #initializeTestedInstance'
]

{ #category : #running }
SpAbstractLayoutTestCase >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #running }
SpAbstractLayoutTestCase >> widget [

	^ self adapter widget
]

{ #category : #accessing }
SpAbstractLayoutTestCase >> window [
	^ window
]

{ #category : #accessing }
SpAbstractLayoutTestCase >> window: aWindowPresenter [ 
	window := aWindowPresenter
]
