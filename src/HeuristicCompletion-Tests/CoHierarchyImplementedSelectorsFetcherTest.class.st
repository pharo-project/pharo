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
		                    (NECSelectorEntry contents: 'ma' node: nil)  fetcher: fetcher; yourself.
		                    (NECSelectorEntry contents: 'mb' node: nil)  fetcher: fetcher; yourself.
		                    (NECSelectorEntry contents: 'mc' node: nil) fetcher: fetcher; yourself.
		                    (NECSelectorEntry contents: 'superma' node: nil)  fetcher: fetcher; yourself.
		                    (NECSelectorEntry contents: 'supermb' node: nil)  fetcher: fetcher; yourself.
		                    (NECSelectorEntry contents: 'supermc' node: nil) fetcher: fetcher; yourself }
]
