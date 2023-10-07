Class {
	#name : 'CoSequenceFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoSequenceFetcherTest >> setUp [

	super setUp.
	fetcher := (CoInstanceVariableFetcher new
			completionClass: self completionClass)
				, (CoGlobalVariableFetcher new
					systemNavigation: self systemNavigation).
	expectedElements := {
		NECInstVarEntry contents: 'a' node: nil.
		NECInstVarEntry contents: 'b' node: nil.
		NECInstVarEntry contents: 'c' node: nil.
		NECGlobalEntry contents: 'A' node: nil.
		NECGlobalEntry contents: 'B' node: nil.
		NECGlobalEntry contents: 'C' node: nil. }
]

{ #category : 'tests' }
CoSequenceFetcherTest >> testCompletionCompositionNextManyShouldStopAfterEndOfElements [

	self assertCollectionOfEntry: (self fetcher next: 10) equals: expectedElements
]

{ #category : 'tests' }
CoSequenceFetcherTest >> testCompletionCompositionNextShouldReturnValuesOfSecondCompletionAfterFirst [

	"Consume instance variables"
	self fetcher next: 3.
	self assertEntry: self fetcher next equals: (NECGlobalEntry contents: 'A' node: nil)
]

{ #category : 'tests' }
CoSequenceFetcherTest >> testNextManyAfterFirstSubfetcherShouldReturnCollectionWithSecondSubfetcherElements [

	self
		assertCollectionOfEntry: (self fetcher next: 5)
		equals: expectedElements allButLast
]
