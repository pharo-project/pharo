Class {
	#name : 'CoGlobalSelectorFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoGlobalSelectorFetcherTest >> setUp [

	super setUp.
	self systemNavigation selectors: #( ma mb mc ab xx ).
	fetcher := CoGlobalSelectorFetcher new.
	fetcher systemNavigation: self systemNavigation.
	fetcher filter: (CoCaseSensitiveBeginsWithFilter filterString: 'm').

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
