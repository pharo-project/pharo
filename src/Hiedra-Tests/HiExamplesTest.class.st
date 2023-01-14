Class {
	#name : #HiExamplesTest,
	#superclass : #TestCase,
	#instVars : [
		'example'
	],
	#category : #'Hiedra-Tests-Model'
}

{ #category : #tests }
HiExamplesTest class >> classWithExamplesToTest [
	^ self
]

{ #category : #testing }
HiExamplesTest class >> isAbstract [
	^ self = HiExamplesTest
]

{ #category : #tests }
HiExamplesTest class >> testSelectors [

	^ self classWithExamplesToTest classSide methods
			select: [ :each |  (each selector beginsWith: 'example') and: [ each numArgs = 0 ] ]
			thenCollect: [ :each | each selector ]
]

{ #category : #tests }
HiExamplesTest class >> whichClassIncludesTestSelector: aSymbol [
	^self classWithExamplesToTest classSide whichClassIncludesSelector: aSymbol
]

{ #category : #private }
HiExamplesTest >> performTest [
	example := self class classWithExamplesToTest perform: testSelector asSymbol
]

{ #category : #running }
HiExamplesTest >> tearDown [

	example ifNotNil: [ example close ].
	super tearDown
]
