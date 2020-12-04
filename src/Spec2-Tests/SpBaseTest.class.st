Class {
	#name : #SpBaseTest,
	#superclass : #TestCase,
	#instVars : [
		'presenter',
		'window'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #testing }
SpBaseTest class >> isAbstract [

	^ self = SpBaseTest
]

{ #category : #accessing }
SpBaseTest >> adapter [

	^ self subclassResponsibility
]

{ #category : #assertions }
SpBaseTest >> assertEvent: anEventName isRaisedInPresenter: aPresenter whenDoing: aBlock [
	
	self
		assertWith: [ :times | times > 0 ]
		timesRaisedEvent: anEventName
		inPresenter: aPresenter
		whenDoing: aBlock
]

{ #category : #assertions }
SpBaseTest >> assertWith: assertionBlock timesRaisedEvent: anEventName inPresenter: aPresenter whenDoing: actionBlock [
	
	| timesCalled |
	timesCalled := 0.
	aPresenter perform: anEventName with: [ timesCalled := timesCalled + 1 ].
	actionBlock value.
	assertionBlock value: timesCalled
]

{ #category : #running }
SpBaseTest >> classToTest [
	self subclassResponsibility
]

{ #category : #assertions }
SpBaseTest >> denyEvent: anEventName isRaisedInPresenter: aPresenter whenDoing: aBlock [
	
	self
		assertWith: [ :times | times = 0 ]
		timesRaisedEvent: anEventName
		inPresenter: aPresenter
		whenDoing: aBlock
]

{ #category : #running }
SpBaseTest >> initializeTestedInstance [
]

{ #category : #running }
SpBaseTest >> openInstance [
	window ifNil: [ window := presenter openWithSpec ]
]

{ #category : #running }
SpBaseTest >> openInstance: aLayoutSelector [
	window := presenter openWithSpec: aLayoutSelector
]

{ #category : #accessing }
SpBaseTest >> presenter [
	^ presenter
]

{ #category : #running }
SpBaseTest >> setUp [
	super setUp.
	presenter := self classToTest new.
	self initializeTestedInstance
]

{ #category : #running }
SpBaseTest >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #tests }
SpBaseTest >> testNewPresenterIsNotBuilt [
	self deny: presenter isBuilt
]

{ #category : #tests }
SpBaseTest >> testNewPresenterIsNotDisplayed [
	self deny: presenter isDisplayed
]

{ #category : #tests }
SpBaseTest >> testNonOpenPresenterDoesNotRaiseBuiltEvent [
	| built |
	built := false.
	presenter whenBuiltDo: [ built := true ].
	self deny: built
]

{ #category : #tests }
SpBaseTest >> testNonOpenPresenterDoesNotRaiseDisplayedEvent [
	| displayed |
	displayed := false.
	presenter whenDisplayDo: [ displayed := true ].
	self deny: displayed
]

{ #category : #tests }
SpBaseTest >> testOpenPresenterIsBuilt [
	self openInstance.
	self assert: presenter isBuilt
]

{ #category : #tests }
SpBaseTest >> testOpenPresenterIsDisplayed [
	self openInstance.
	self assert: presenter isDisplayed
]

{ #category : #tests }
SpBaseTest >> testOpenPresenterRaisesBuiltEvent [
	| built |
	built := false.
	presenter whenBuiltDo: [ built := true ].
	self openInstance.
	self assert: built
]

{ #category : #tests }
SpBaseTest >> testOpenPresenterRaisesDisplayEvent [
	| displayed |
	displayed := false.
	presenter whenDisplayDo: [ displayed := true ].
	self openInstance.
	self assert: displayed
]

{ #category : #accessing }
SpBaseTest >> widget [
	
	^ self adapter widget
]
