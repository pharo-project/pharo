Class {
	#name : #SpTestCase,
	#superclass : #TestCase,
	#instVars : [
		'window',
		'presenter'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #testing }
SpTestCase class >> isAbstract [

	^ self == SpTestCase
]

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

{ #category : #tests }
SpTestCase >> testNewPresenterIsNotBuilt [

	self deny: presenter isBuilt
]

{ #category : #tests }
SpTestCase >> testNewPresenterIsNotDisplayed [

	self deny: presenter isDisplayed
]

{ #category : #tests }
SpTestCase >> testNonOpenPresenterDoesNotRaiseBuiltEvent [

	| built |
	built := false.
	presenter whenBuiltDo: [ built := true ].
	self deny: built
]

{ #category : #tests }
SpTestCase >> testNonOpenPresenterDoesNotRaiseDisplayedEvent [

	| displayed |
	displayed := false.
	presenter whenDisplayDo: [ displayed := true ].
	self deny: displayed
]

{ #category : #tests }
SpTestCase >> testOpenPresenterIsBuilt [

	self openInstance.
	self assert: presenter isBuilt
]

{ #category : #tests }
SpTestCase >> testOpenPresenterIsDisplayed [

	self openInstance.
	self assert: presenter isDisplayed
]

{ #category : #tests }
SpTestCase >> testOpenPresenterRaisesBuiltEvent [

	| built |
	built := false.
	presenter whenBuiltDo: [ built := true ].
	self openInstance.
	self assert: built
]

{ #category : #tests }
SpTestCase >> testOpenPresenterRaisesDisplayEvent [

	| displayed |
	displayed := false.
	presenter whenDisplayDo: [ displayed := true ].
	self openInstance.
	self assert: displayed
]

{ #category : #accessing }
SpTestCase >> widget [
	
	^ self adapter widget
]
