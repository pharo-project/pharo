"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'CoMockFetcherToRecordResets',
	#superclass : 'CoFetcher',
	#instVars : [
		'resetingCount'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'enumerating' }
CoMockFetcherToRecordResets >> entriesDo: aBlock [

	1 to: 1000 do: [ :anIndex | (NECGlobalEntry contents: anIndex printString node: nil) ]
]

{ #category : 'initialization' }
CoMockFetcherToRecordResets >> initialize [

	super initialize.
	resetingCount := 0
]

{ #category : 'initialization' }
CoMockFetcherToRecordResets >> reset [

	resetingCount := resetingCount + 1.
	super reset
]

{ #category : 'accessing' }
CoMockFetcherToRecordResets >> resetingCount [

	^ resetingCount
]
