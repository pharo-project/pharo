Class {
	#name : #SpecTestCase,
	#superclass : #TestCase,
	#instVars : [
		'window',
		'presenter'
	],
	#category : #'Spec-Tests-Utils'
}

{ #category : #assertions }
SpecTestCase >> assertEvent: anEventName isRaisedInPresenter: aPresenter whenDoing: aBlock [
	
	self
		assertWith: [ :times | times > 0 ]
		timesRaisedEvent: anEventName
		inPresenter: aPresenter
		whenDoing: aBlock
]

{ #category : #assertions }
SpecTestCase >> assertWith: assertionBlock timesRaisedEvent: anEventName inPresenter: aPresenter whenDoing: actionBlock [
	
	| timesCalled |
	timesCalled := 0.
	aPresenter perform: anEventName with: [ timesCalled := timesCalled + 1 ].
	actionBlock value.
	assertionBlock value: timesCalled
]

{ #category : #running }
SpecTestCase >> classToTest [
	self subclassResponsibility
]

{ #category : #assertions }
SpecTestCase >> denyEvent: anEventName isRaisedInPresenter: aPresenter whenDoing: aBlock [
	
	self
		assertWith: [ :times | times = 0 ]
		timesRaisedEvent: anEventName
		inPresenter: aPresenter
		whenDoing: aBlock
]

{ #category : #running }
SpecTestCase >> initializeTestedInstance [
]

{ #category : #running }
SpecTestCase >> openInstance [
	window ifNil: [ window := presenter openWithSpec ]
]

{ #category : #running }
SpecTestCase >> openInstance: aLayoutSelector [
	window := presenter openWithSpec: aLayoutSelector
]

{ #category : #accessing }
SpecTestCase >> presenter [
	^ presenter
]

{ #category : #running }
SpecTestCase >> setUp [
	super setUp.
	presenter := self classToTest new.
	self initializeTestedInstance
]

{ #category : #running }
SpecTestCase >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #accessing }
SpecTestCase >> widget [
	
	^ self adapter widget
]
