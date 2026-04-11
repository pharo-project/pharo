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
		                    ((NECSelectorEntry contents: 'ma' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((NECSelectorEntry contents: 'mb' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((NECSelectorEntry contents: 'mc' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((NECSelectorEntry contents: 'superma' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((NECSelectorEntry contents: 'supermb' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself).
		                    ((NECSelectorEntry contents: 'supermc' node: nil)
			                     fetcherName: fetcher class name;
			                     yourself) }
]
