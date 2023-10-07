Class {
	#name : 'CoBasicFetcherTest',
	#superclass : 'CoBasicCompletionTest',
	#instVars : [
		'fetcher'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'testing' }
CoBasicFetcherTest class >> isAbstract [

	^ self == CoBasicFetcherTest
]

{ #category : 'accessing' }
CoBasicFetcherTest >> fetcher [
	^ fetcher
]

{ #category : 'tests' }
CoBasicFetcherTest >> testConsumedFetcherShouldBeAtEnd [

	self fetcher upToEnd.
	self assert: self fetcher atEnd
]

{ #category : 'tests' }
CoBasicFetcherTest >> testNextShouldReturnNilAfterEnd [

	self fetcher upToEnd.
	self assert: self fetcher next equals: nil
]
