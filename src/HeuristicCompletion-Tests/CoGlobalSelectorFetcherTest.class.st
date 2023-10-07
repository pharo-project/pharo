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
	self systemNavigation selectors: #(ma mb mc ab xx).
	fetcher := CoGlobalSelectorFetcher new.
	fetcher systemNavigation: self systemNavigation.
	fetcher filter: (CoCaseSensitiveBeginsWithFilter filterString: 'm').

	expectedElements := {
		NECSelectorEntry contents: 'ma' node: nil.
		NECSelectorEntry contents: 'mb' node: nil.
		NECSelectorEntry contents: 'mc' node: nil. }
]
