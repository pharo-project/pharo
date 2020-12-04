Class {
	#name : #CoRepeatedHierarchyImplementedSelectorsFetcherTest,
	#superclass : #CoImplementedSelectorsFetcherTest,
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #running }
CoRepeatedHierarchyImplementedSelectorsFetcherTest >> setUp [

	super setUp.
	fetcher := (CoClassImplementedMessagesFetcher new
		completionClass: self completionClass;
		forHierarchy)
		withoutRepetition.
	self completionClass superclass
		selectors: (self completionClass superclass selectors copyWith: 'ma').
	expectedElements := { 
		NECSelectorEntry contents: 'ma' node: nil.
		NECSelectorEntry contents: 'mb' node: nil.
		NECSelectorEntry contents: 'mc' node: nil.
		NECSelectorEntry contents: 'superma' node: nil.
		NECSelectorEntry contents: 'supermb' node: nil.
		NECSelectorEntry contents: 'supermc' node: nil.}
]
