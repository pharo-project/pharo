Class {
	#name : #CoNarrowHistoryFetcherTest,
	#superclass : #CoBasicFetcherWithElementsTest,
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #initialization }
CoNarrowHistoryFetcherTest >> setUp [

	super setUp.
	fetcher := CoNarrowHistoryFetcher new
		decoree: (CoCollectionFetcher onCollection: #( a b c )).
	expectedElements := #(a b c).
]

{ #category : #tests }
CoNarrowHistoryFetcherTest >> testNarrowingAndUnnarrowingReturnsSameResult [

	| f1Result f2Result f3Result |

	fetcher := (CoCollectionFetcher onCollection: Symbol selectorTable)
		           withNarrowHistory.
	fetcher := fetcher
		           narrowFilterBlock: [ :e | e beginsWith: 'as' ]
		           narrowKey: 'as'.
	f1Result := fetcher next: 10.

	fetcher := fetcher
		           narrowFilterBlock: [ :e | e beginsWith: 'asC' ]
		           narrowKey: 'asC'.
	f2Result := fetcher next: 10.

	fetcher := fetcher
		      unnarrowFilterBlock: [ :e | e beginsWith: 'as' ]
		      narrowKey: 'as'.
	f3Result := fetcher next: 10.

	self assertCollection: f1Result equals: f3Result 
]

{ #category : #tests }
CoNarrowHistoryFetcherTest >> testNarrowingReturnsSameElementsThatCallingDirectly [

	| originalSearch narrowedResults |
	
	"First execution with complete query"
	fetcher := (CoCollectionFetcher onCollection: Symbol selectorTable)
		           withNarrowHistory.
	fetcher := fetcher
		           narrowFilterBlock: [ :e | e beginsWith: 'asL' ]
		           narrowKey: 'as'.
	originalSearch := fetcher next: 10.

	"Second execution with query using narrowing"
	fetcher := (CoCollectionFetcher onCollection: Symbol selectorTable)
		           withNarrowHistory.
	fetcher := fetcher
		           narrowFilterBlock: [ :e | e beginsWith: 'as' ]
		           narrowKey: 'as'.
	fetcher next: 10.
	fetcher := fetcher
		           narrowFilterBlock: [ :e | e beginsWith: 'asL' ]
		           narrowKey: 'asL'.

	narrowedResults := fetcher next: 10.
	
	self assertCollection: originalSearch equals: narrowedResults
]

{ #category : #tests }
CoNarrowHistoryFetcherTest >> testUnnarrowingAndThenNarrowingReturnsSameResult [


	| originalResults newResults |
	fetcher := (CoCollectionFetcher onCollection: Symbol selectorTable)
		           withNarrowHistory.
	fetcher := fetcher
		           narrowFilterBlock: [ :e | e beginsWith: 'as' ]
		           narrowKey: 'as'.
	fetcher upToEnd.

	fetcher := fetcher
		           narrowFilterBlock: [ :e | e beginsWith: 'asL' ]
		           narrowKey: 'asC'.
	originalResults := fetcher upToEnd.

	fetcher := fetcher
		      unnarrowFilterBlock: [ :e | e beginsWith: 'as' ]
		      narrowKey: 'as'.
	fetcher upToEnd.

	fetcher := fetcher
		           narrowFilterBlock: [ :e | e beginsWith: 'asL' ]
		           narrowKey: 'asC'.
	newResults := fetcher upToEnd.

	self assertCollection: originalResults equals: newResults 
]
