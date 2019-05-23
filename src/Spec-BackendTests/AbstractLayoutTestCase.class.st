Class {
	#name : #AbstractLayoutTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'presenter',
		'window',
		'backendForTest'
	],
	#category : #'Spec-BackendTests'
}

{ #category : #testing }
AbstractLayoutTestCase class >> isAbstract [ 
	^ self = AbstractLayoutTestCase
]

{ #category : #testing }
AbstractLayoutTestCase class >> testParameters [
	^ ParametrizedTestMatrix new
		forSelector: #backendForTest addOptions: AbstractBackendForTest allSubclasses;
		yourself
]

{ #category : #running }
AbstractLayoutTestCase >> adapter [

	"Force opening the spec instance here.
	The action should have been correctly configured before
	depending on the spec initialization strategy"
	backendForTest openInstanceOf: self.
	^ presenter adapter
]

{ #category : #accessing }
AbstractLayoutTestCase >> backendForTest: aClass [ 
	
	backendForTest := aClass new
]

{ #category : #accessing }
AbstractLayoutTestCase >> classToTest [

	^ self subclassResponsibility
]

{ #category : #running }
AbstractLayoutTestCase >> openInstance [

	backendForTest openInstanceOf: self 

]

{ #category : #running }
AbstractLayoutTestCase >> prepareToRunAgain [

	backendForTest runTest: [ super prepareToRunAgain ]

]

{ #category : #accessing }
AbstractLayoutTestCase >> presenter [
	^ presenter
]

{ #category : #running }
AbstractLayoutTestCase >> runCase [

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
AbstractLayoutTestCase >> setUp [
	super setUp.	"This super is useless but it's to make the release tests happy."
	self error: 'Do not override me.... use #initializeTestedInstance'
]

{ #category : #running }
AbstractLayoutTestCase >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #running }
AbstractLayoutTestCase >> widget [

	^ self adapter widget
]

{ #category : #accessing }
AbstractLayoutTestCase >> window [
	^ window
]

{ #category : #accessing }
AbstractLayoutTestCase >> window: aWindowPresenter [ 
	window := aWindowPresenter
]
