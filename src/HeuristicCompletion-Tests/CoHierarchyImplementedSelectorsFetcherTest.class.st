Class {
	#name : 'CoHierarchyImplementedSelectorsFetcherTest',
	#superclass : 'CoImplementedSelectorsFetcherTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoHierarchyImplementedSelectorsFetcherTest >> setUp [

	super setUp.
	fetcher := CoClassImplementedMessagesFetcher new
		           completionClass: self completionClass;
		           forHierarchy.
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
