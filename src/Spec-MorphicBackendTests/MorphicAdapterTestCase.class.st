Class {
	#name : #MorphicAdapterTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'presenter',
		'specInitializationStrategy',
		'window'
	],
	#category : #'Spec-MorphicBackendTests'
}

{ #category : #testing }
MorphicAdapterTestCase class >> isAbstract [ 
	^ self == MorphicAdapterTestCase
]

{ #category : #testing }
MorphicAdapterTestCase class >> testParameters [ 

	^ ParametrizedTestMatrix new
		forSelector: #specInitializationStrategy
		addOptions: { [ SpecInitializationStrategy beforeTest ]. [ SpecInitializationStrategy afterTest ] }
]

{ #category : #running }
MorphicAdapterTestCase >> adapter [

	^ presenter adapter
]

{ #category : #running }
MorphicAdapterTestCase >> initializeTestedInstance [
]

{ #category : #running }
MorphicAdapterTestCase >> openInstance [

	window ifNil: [ window := presenter openWithSpec ].
	World doOneCycle.
]

{ #category : #private }
MorphicAdapterTestCase >> performTest [
	
	specInitializationStrategy beforeTest: self.
	super performTest.
]

{ #category : #accessing }
MorphicAdapterTestCase >> presenter [
	^ presenter
]

{ #category : #running }
MorphicAdapterTestCase >> setUp [
	super setUp.
	presenter := self classToTest new.
	self initializeTestedInstance
]

{ #category : #accessing }
MorphicAdapterTestCase >> specInitializationStrategy: aStrategy [
	
	specInitializationStrategy := aStrategy
]

{ #category : #running }
MorphicAdapterTestCase >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #tests }
MorphicAdapterTestCase >> testSmoke [

	self openInstance
]

{ #category : #running }
MorphicAdapterTestCase >> widget [

	"Force opening the spec instance here.
	The action should have been correctly configured before
	depending on the spec initialization strategy"
	self openInstance.
	World doOneCycle.
	^ self adapter widget
]
