Class {
	#name : #CoHierarchyImplementedSelectorsFetcherTest,
	#superclass : #CoImplementedSelectorsFetcherTest,
	#category : #'HeuristicCompletion-Tests'
}

{ #category : #running }
CoHierarchyImplementedSelectorsFetcherTest >> setUp [

	super setUp.
	fetcher := CoClassImplementedMessagesFetcher new
		completionClass: self completionClass;
		forHierarchy.
	expectedElements := { 
		NECSelectorEntry contents: 'ma' node: nil.
		NECSelectorEntry contents: 'mb' node: nil.
		NECSelectorEntry contents: 'mc' node: nil.
		NECSelectorEntry contents: 'superma' node: nil.
		NECSelectorEntry contents: 'supermb' node: nil.
		NECSelectorEntry contents: 'supermc' node: nil.}
]
