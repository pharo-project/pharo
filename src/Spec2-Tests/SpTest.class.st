Class {
	#name : #SpTest,
	#superclass : #TestCase,
	#instVars : [
		'window',
		'presenter'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #testing }
SpTest class >> isAbstract [
	^ self = SpTest
]

{ #category : #assertions }
SpTest >> assertEvent: anEventName isRaisedInPresenter: aPresenter whenDoing: aBlock [
	
	self
		assertWith: [ :times | times > 0 ]
		timesRaisedEvent: anEventName
		inPresenter: aPresenter
		whenDoing: aBlock
]

{ #category : #assertions }
SpTest >> assertWith: assertionBlock timesRaisedEvent: anEventName inPresenter: aPresenter whenDoing: actionBlock [
	
	| timesCalled |
	timesCalled := 0.
	aPresenter perform: anEventName with: [ timesCalled := timesCalled + 1 ].
	actionBlock value.
	assertionBlock value: timesCalled
]

{ #category : #running }
SpTest >> classToTest [
	self subclassResponsibility
]

{ #category : #assertions }
SpTest >> denyEvent: anEventName isRaisedInPresenter: aPresenter whenDoing: aBlock [
	
	self
		assertWith: [ :times | times = 0 ]
		timesRaisedEvent: anEventName
		inPresenter: aPresenter
		whenDoing: aBlock
]

{ #category : #running }
SpTest >> initializeTestedInstance [
]

{ #category : #running }
SpTest >> openInstance [
	window ifNil: [ window := presenter openWithSpec ]
]

{ #category : #running }
SpTest >> openInstance: aLayoutSelector [
	window := presenter openWithSpec: aLayoutSelector
]

{ #category : #accessing }
SpTest >> presenter [
	^ presenter
]

{ #category : #running }
SpTest >> setUp [
	super setUp.
	presenter := self classToTest new.
	self initializeTestedInstance
]

{ #category : #running }
SpTest >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #tests }
SpTest >> testNewPresenterIsNotBuilt [
	self deny: presenter isBuilt
]

{ #category : #tests }
SpTest >> testNewPresenterIsNotDisplayed [
	self deny: presenter isDisplayed
]

{ #category : #tests }
SpTest >> testNonOpenPresenterDoesNotRaiseBuiltEvent [
	| built |
	built := false.
	presenter whenBuiltDo: [ built := true ].
	self deny: built
]

{ #category : #tests }
SpTest >> testNonOpenPresenterDoesNotRaiseDisplayedEvent [
	| displayed |
	displayed := false.
	presenter whenDisplayDo: [ displayed := true ].
	self deny: displayed
]

{ #category : #tests }
SpTest >> testOpenPresenterIsBuilt [
	self openInstance.
	self assert: presenter isBuilt
]

{ #category : #tests }
SpTest >> testOpenPresenterIsDisplayed [
	self openInstance.
	self assert: presenter isDisplayed
]

{ #category : #tests }
SpTest >> testOpenPresenterRaisesBuiltEvent [
	| built |
	built := false.
	presenter whenBuiltDo: [ built := true ].
	self openInstance.
	self assert: built
]

{ #category : #tests }
SpTest >> testOpenPresenterRaisesDisplayEvent [
	| displayed |
	displayed := false.
	presenter whenDisplayDo: [ displayed := true ].
	self openInstance.
	self assert: displayed
]

{ #category : #accessing }
SpTest >> widget [
	
	^ self adapter widget
]
