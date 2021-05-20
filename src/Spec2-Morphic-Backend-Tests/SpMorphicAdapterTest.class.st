Class {
	#name : #SpMorphicAdapterTest,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'presenter',
		'specInitializationStrategy',
		'window'
	],
	#category : #'Spec2-Morphic-Backend-Tests'
}

{ #category : #testing }
SpMorphicAdapterTest class >> isAbstract [ 
	^ self == SpMorphicAdapterTest
]

{ #category : #testing }
SpMorphicAdapterTest class >> testParameters [ 

	^ ParametrizedTestMatrix new
		forSelector: #specInitializationStrategy
		addOptions: { [ SpInitializationStrategy beforeTest ]. [ SpInitializationStrategy afterTest ] }
]

{ #category : #running }
SpMorphicAdapterTest >> adapter [

	^ presenter adapter
]

{ #category : #running }
SpMorphicAdapterTest >> initializeTestedInstance [
]

{ #category : #running }
SpMorphicAdapterTest >> openInstance [

	window ifNil: [ window := presenter openWithSpec ].
	MorphicRenderLoop new doOneCycle.
]

{ #category : #private }
SpMorphicAdapterTest >> performTest [
	
	specInitializationStrategy beforeTest: self.
	super performTest.
]

{ #category : #accessing }
SpMorphicAdapterTest >> presenter [
	^ presenter
]

{ #category : #running }
SpMorphicAdapterTest >> setUp [
	super setUp.
	presenter := self classToTest new.
	self initializeTestedInstance
]

{ #category : #accessing }
SpMorphicAdapterTest >> specInitializationStrategy: aStrategy [
	
	specInitializationStrategy := aStrategy
]

{ #category : #running }
SpMorphicAdapterTest >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #running }
SpMorphicAdapterTest >> widget [

	"Force opening the spec instance here.
	The action should have been correctly configured before
	depending on the spec initialization strategy"
	self openInstance.
	MorphicRenderLoop new doOneCycle.
	^ self adapter widget
]
