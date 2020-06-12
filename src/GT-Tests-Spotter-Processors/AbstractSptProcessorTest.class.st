Class {
	#name : #AbstractSptProcessorTest,
	#superclass : #TestCase,
	#instVars : [
		'text',
		'candidates',
		'context',
		'processor',
		'stream'
	],
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #testing }
AbstractSptProcessorTest class >> isAbstract [ 

	^ self == AbstractSptProcessorTest
]

{ #category : #asserting }
AbstractSptProcessorTest >> assertQuantityOfResults: anInteger [ 
	
	self assert: candidates results size equals: anInteger
]

{ #category : #asserting }
AbstractSptProcessorTest >> assertResultsIncludes: anElement [ 
	
	self assert: ((candidates results collect: [:each | each content]) includes: anElement)
]

{ #category : #asserting }
AbstractSptProcessorTest >> denyResultsIncludes: anElement [ 

	self deny: ((candidates results collect: [:each | each content]) includes: anElement )
]

{ #category : #tests }
AbstractSptProcessorTest >> processor [

	^ self subclassResponsibility 
]

{ #category : #testing }
AbstractSptProcessorTest >> rerunWithText: aText [ 
	
	text := aText.
	context search: aText.
	context text: aText.
	candidates reset.
	processor filterInContext: context
]

{ #category : #tests }
AbstractSptProcessorTest >> runForText: aText [

	candidates := GTMockCandidatesList new.
	text := aText.
	stream := GTMockSpotterStream new
		receiver: candidates;
		yourself.

	context:= GTSpotterContext new 
		step: self;
		stream: stream;
		text: text;
		search: text;
		yourself.

	processor := self processor.
			
	processor filterInContext: context.
]

{ #category : #running }
AbstractSptProcessorTest >> setUp [ 
	
	super setUp.
	SptDefaultSourceFactory forTest.
]

{ #category : #running }
AbstractSptProcessorTest >> tearDown [ 

	SptSourceFactory reset.
	super tearDown.
]

{ #category : #tests }
AbstractSptProcessorTest >> testSearchingInvalidTextDoesNotReturnResult [

	self runForText: 'assdsadasdsada'.
	self assert: candidates results isEmpty
]
