Class {
	#name : #SpAbstractLayoutTest,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'presenter',
		'window',
		'backendForTest'
	],
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractLayoutTest class >> isAbstract [ 
	^ self = SpAbstractLayoutTest
]

{ #category : #testing }
SpAbstractLayoutTest class >> testParameters [
	^ ParametrizedTestMatrix new
		forSelector: #backendForTest addOptions: SpAbstractBackendForTest allSubclasses;
		yourself
]

{ #category : #running }
SpAbstractLayoutTest >> adapter [

	"Force opening the spec instance here.
	The action should have been correctly configured before
	depending on the spec initialization strategy"
	backendForTest openInstanceOf: self.
	^ presenter adapter
]

{ #category : #accessing }
SpAbstractLayoutTest >> backendForTest: aClass [ 
	
	backendForTest := aClass new
]

{ #category : #accessing }
SpAbstractLayoutTest >> classToTest [

	^ self subclassResponsibility
]

{ #category : #running }
SpAbstractLayoutTest >> openInstance [

	backendForTest openInstanceOf: self 

]

{ #category : #running }
SpAbstractLayoutTest >> prepareToRunAgain [

	backendForTest runTest: [ super prepareToRunAgain ]

]

{ #category : #accessing }
SpAbstractLayoutTest >> presenter [
	^ presenter
]

{ #category : #running }
SpAbstractLayoutTest >> runCase [

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
SpAbstractLayoutTest >> setUp [
	super setUp.	"This super is useless but it's to make the release tests happy."
	self error: 'Do not override me.... use #initializeTestedInstance'
]

{ #category : #running }
SpAbstractLayoutTest >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #running }
SpAbstractLayoutTest >> widget [

	^ self adapter widget
]

{ #category : #accessing }
SpAbstractLayoutTest >> window [
	^ window
]

{ #category : #accessing }
SpAbstractLayoutTest >> window: aWindowPresenter [ 
	window := aWindowPresenter
]
