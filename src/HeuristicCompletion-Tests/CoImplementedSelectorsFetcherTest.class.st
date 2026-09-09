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
		                    ((CoSelectorEntry contents: 'ma' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((CoSelectorEntry contents: 'mb' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((CoSelectorEntry contents: 'mc' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself) }
]
