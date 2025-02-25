Class {
	#name : 'CoResultSetTest',
	#superclass : 'CoBasicCompletionTest',
	#instVars : [
		'completion'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoResultSetTest >> setUp [

	super setUp.
	self completionClass addInstanceVariable: 'ba'.
	completion := CoResultSet
		fetcher: (CoInstanceVariableFetcher new
			completionClass: self completionClass;
			yourself)
]

{ #category : 'tests' }
CoResultSetTest >> testBroadenFilterAfterFetchCompletionShouldBroadenResults [

	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'ba').
	completion fetch: 1.
	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'b').

	self assertEntry: completion first equals: (NECInstVarEntry contents: 'b' node: nil)
]

{ #category : 'tests' }
CoResultSetTest >> testBroadenFilterAfterFetchCompletionShouldBroadenResultsWhenCaseInsensitve [

	completion replaceFilterWith: (CoCaseInsensitiveBeginsWithFilter filterString: 'BA').
	completion fetch: 1.
	completion replaceFilterWith: (CoCaseInsensitiveBeginsWithFilter filterString: 'B').

	self
		assertEntry: completion first
		equals: (NECInstVarEntry contents: 'b' node: nil)
]

{ #category : 'tests' }
CoResultSetTest >> testBroaderFilterCompletionShouldBroadenResults [

	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'ba').
	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'b').

	self assertEntry: completion first equals: (NECInstVarEntry contents: 'b' node: nil)
]

{ #category : 'tests' }
CoResultSetTest >> testBroaderFilterCompletionShouldBroadenResultsWhenCaseInsensitive [

	completion replaceFilterWith: (CoCaseInsensitiveBeginsWithFilter filterString: 'ba').
	completion replaceFilterWith: (CoCaseInsensitiveBeginsWithFilter filterString: 'B').

	self assertEntry: completion first equals: (NECInstVarEntry contents: 'b' node: nil)
]

{ #category : 'tests' }
CoResultSetTest >> testCompletionWithElementsToFetchShouldHaveMore [

	self assert: completion hasMoreElements
]

{ #category : 'tests' }
CoResultSetTest >> testCompletionWithNoElementsToFetchShouldNotHaveMore [

	| c |
	c := CoResultSet fetcher: (CoInstanceVariableFetcher new
			completionClass: self completionClass).
	c fetchAll.
	self deny: c hasMoreElements
]

{ #category : 'tests' }
CoResultSetTest >> testEnumerateShouldFetch [

	self assertEntry: completion first equals: (NECInstVarEntry contents: 'a' node: nil)
]

{ #category : 'tests' }
CoResultSetTest >> testFilterCompletionAfterFetchingShouldNarrowResults [

	completion fetch: 1.
	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'b').

	self assertEntry: completion first equals: (NECInstVarEntry contents: 'b' node: nil)
]

{ #category : 'tests' }
CoResultSetTest >> testFilterTwiceWithSameCompletionShouldReturnSameResults [

	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'ba').
	completion fetch: 2.
	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'ba').

	self assertCollectionOfEntry: (completion first: 2) equals: { NECInstVarEntry contents: 'ba' node: nil }
]

{ #category : 'tests' }
CoResultSetTest >> testNarrowFilterAfterFetchCompletionShouldNarrowResults [

	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'b').
	completion fetch: 1.
	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'ba').

	self assertEntry: completion first equals: (NECInstVarEntry contents: 'ba' node: nil)
]

{ #category : 'tests' }
CoResultSetTest >> testNarrowerFilterCompletionShouldNarrowResults [

	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'b').
	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'ba').

	self assertEntry: completion first equals: (NECInstVarEntry contents: 'ba' node: nil)
]

{ #category : 'tests' }
CoResultSetTest >> testResetFilterShouldRetrievePreviousResults [

	completion replaceFilterWith: (CoCaseSensitiveBeginsWithFilter filterString: 'b').
	completion fetch: 5.
	completion resetFilter.

	self assertEntry: completion first equals: (NECInstVarEntry contents: 'a' node: nil)
]

{ #category : 'tests' }
CoResultSetTest >> testResetFilterWithNoFilterSetShouldNotAlterResults [

	completion resetFilter.

	self assertEntry: completion first equals: (NECInstVarEntry contents: 'a' node: nil)
]
