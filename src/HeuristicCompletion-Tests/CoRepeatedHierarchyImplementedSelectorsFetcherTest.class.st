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
		                    (NECSelectorEntry contents: 'ma' node: nil) fetcher: fetcher; yourself.
		                    (NECSelectorEntry contents: 'mb' node: nil) fetcher: fetcher; yourself.
		                    (NECSelectorEntry contents: 'mc' node: nil) fetcher: fetcher; yourself.
		                    (NECSelectorEntry contents: 'superma' node: nil) fetcher: fetcher; yourself.
		                    (NECSelectorEntry contents: 'supermb' node: nil) fetcher: fetcher; yourself.
		                    (NECSelectorEntry contents: 'supermc' node: nil)  fetcher: fetcher; yourself}
]
