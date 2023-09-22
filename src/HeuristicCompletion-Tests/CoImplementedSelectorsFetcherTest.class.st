Class {
	#name : 'CoImplementedSelectorsFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoImplementedSelectorsFetcherTest >> setUp [

	super setUp.
	fetcher := CoClassImplementedMessagesFetcher new.
	fetcher completionClass: self completionClass.
	expectedElements := {
		NECSelectorEntry contents: 'ma' node: nil.
		NECSelectorEntry contents: 'mb' node: nil.
		NECSelectorEntry contents: 'mc' node: nil.}
]
