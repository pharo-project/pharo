Class {
	#name : #SpMorphicAdapterTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'presenter',
		'specInitializationStrategy',
		'window'
	],
	#category : #'Spec2-Morphic-Backend-Tests'
}

{ #category : #testing }
SpMorphicAdapterTestCase class >> isAbstract [ 
	^ self == SpMorphicAdapterTestCase
]

{ #category : #testing }
SpMorphicAdapterTestCase class >> testParameters [ 

	^ ParametrizedTestMatrix new
		forSelector: #specInitializationStrategy
		addOptions: { [ SpInitializationStrategy beforeTest ]. [ SpInitializationStrategy afterTest ] }
]

{ #category : #running }
SpMorphicAdapterTestCase >> adapter [

	^ presenter adapter
]

{ #category : #running }
SpMorphicAdapterTestCase >> initializeTestedInstance [
]

{ #category : #running }
SpMorphicAdapterTestCase >> openInstance [

	window ifNil: [ window := presenter openWithSpec ].
	self currentWorld doOneCycle.
]

{ #category : #private }
SpMorphicAdapterTestCase >> performTest [
	
	specInitializationStrategy beforeTest: self.
	super performTest.
]

{ #category : #accessing }
SpMorphicAdapterTestCase >> presenter [
	^ presenter
]

{ #category : #running }
SpMorphicAdapterTestCase >> setUp [
	super setUp.
	presenter := self classToTest new.
	self initializeTestedInstance
]

{ #category : #accessing }
SpMorphicAdapterTestCase >> specInitializationStrategy: aStrategy [
	
	specInitializationStrategy := aStrategy
]

{ #category : #running }
SpMorphicAdapterTestCase >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #running }
SpMorphicAdapterTestCase >> widget [

	"Force opening the spec instance here.
	The action should have been correctly configured before
	depending on the spec initialization strategy"
	self openInstance.
	self currentWorld doOneCycle.
	^ self adapter widget
]
