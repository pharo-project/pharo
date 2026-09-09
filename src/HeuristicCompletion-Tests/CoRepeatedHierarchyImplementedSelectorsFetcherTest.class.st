Class {
	#name : 'CoRepeatedHierarchyImplementedSelectorsFetcherTest',
	#superclass : 'CoImplementedSelectorsFetcherTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoRepeatedHierarchyImplementedSelectorsFetcherTest >> setUp [

	super setUp.
	fetcher := (CoClassImplementedMessagesFetcher new
		            completionClass: self completionClass;
		            forHierarchy) withoutRepetition withNarrowHistory.
	self completionClass superclass selectors: (self completionClass superclass selectors copyWith: 'ma').
	expectedElements := {
		                    ((CoSelectorEntry contents: 'ma' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((CoSelectorEntry contents: 'mb' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((CoSelectorEntry contents: 'mc' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((CoSelectorEntry contents: 'superma' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((CoSelectorEntry contents: 'supermb' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((CoSelectorEntry contents: 'supermc' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself) }
]
