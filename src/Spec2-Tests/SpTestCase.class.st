Class {
	#name : #SpTestCase,
	#superclass : #TestCase,
	#instVars : [
		'window',
		'presenter'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #assertions }
SpTestCase >> assertEvent: anEventName isRaisedInPresenter: aPresenter whenDoing: aBlock [
	
	self
		assertWith: [ :times | times > 0 ]
		timesRaisedEvent: anEventName
		inPresenter: aPresenter
		whenDoing: aBlock
]

{ #category : #assertions }
SpTestCase >> assertWith: assertionBlock timesRaisedEvent: anEventName inPresenter: aPresenter whenDoing: actionBlock [
	
	| timesCalled |
	timesCalled := 0.
	aPresenter perform: anEventName with: [ timesCalled := timesCalled + 1 ].
	actionBlock value.
	assertionBlock value: timesCalled
]

{ #category : #running }
SpTestCase >> classToTest [
	self subclassResponsibility
]

{ #category : #assertions }
SpTestCase >> denyEvent: anEventName isRaisedInPresenter: aPresenter whenDoing: aBlock [
	
	self
		assertWith: [ :times | times = 0 ]
		timesRaisedEvent: anEventName
		inPresenter: aPresenter
		whenDoing: aBlock
]

{ #category : #running }
SpTestCase >> initializeTestedInstance [
]

{ #category : #running }
SpTestCase >> openInstance [
	window ifNil: [ window := presenter openWithSpec ]
]

{ #category : #running }
SpTestCase >> openInstance: aLayoutSelector [
	window := presenter openWithSpec: aLayoutSelector
]

{ #category : #accessing }
SpTestCase >> presenter [
	^ presenter
]

{ #category : #running }
SpTestCase >> setUp [
	super setUp.
	presenter := self classToTest new.
	self initializeTestedInstance
]

{ #category : #running }
SpTestCase >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #accessing }
SpTestCase >> widget [
	
	^ self adapter widget
]
