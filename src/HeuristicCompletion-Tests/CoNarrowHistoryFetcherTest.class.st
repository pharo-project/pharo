Class {
	#name : 'CoNarrowHistoryFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoNarrowHistoryFetcherTest >> setUp [

	super setUp.
	fetcher := CoNarrowHistoryFetcher new
		decoree: (CoCollectionFetcher onCollection: #( a b c )).
	expectedElements := #(a b c)
]

{ #category : 'tests' }
CoNarrowHistoryFetcherTest >> testNarrowingAndUnnarrowingReturnsSameResult [

	| f1Result f2Result f3Result |

	fetcher := (CoCollectionFetcher onCollection: #((
		as1
		asCol
		as2
		asCC
		as3
		asCCC
	) collect: [ :s | #CoSelectorEntry new contents: s ])) withNarrowHistory.
	fetcher := fetcher
		           narrowFilter: (CoCaseSensitiveBeginsWithFilter filterString: 'as')
		           narrowKey: 'as'.
	f1Result := fetcher next: 10.

	fetcher := fetcher
		           narrowFilter: (CoCaseSensitiveBeginsWithFilter filterString: 'asC')
		           narrowKey: 'asC'.
	f2Result := fetcher next: 10.

	fetcher := fetcher
		      unnarrowFilter: (CoCaseSensitiveBeginsWithFilter filterString: 'as')
		      narrowKey: 'as'.
	f3Result := fetcher next: 10.

	self assertCollection: f1Result equals: f3Result
]

{ #category : 'tests' }
CoNarrowHistoryFetcherTest >> testNarrowingReturnsSameElementsThatCallingDirectly [

	| originalSearch narrowedResults |

	"First execution with complete query"
	fetcher := (CoCollectionFetcher onCollection: #((
		as1
		asCol
		as2
		asCC
		as3
		asCCC
	) collect: [ :s | #CoSelectorEntry new contents: s ])) withNarrowHistory.
	fetcher := fetcher narrowFilter: (CoCaseSensitiveBeginsWithFilter filterString: 'asL') narrowKey: 'as'.
	originalSearch := fetcher next: 10.

	"Second execution with query using narrowing"
	fetcher := (CoCollectionFetcher onCollection: #((
		as1
		asCol
		as2
		asCC
		as3
		asCCC
	) collect: [ :s | #CoSelectorEntry new contents: s ])) withNarrowHistory.
	fetcher := fetcher narrowFilter: (CoCaseSensitiveBeginsWithFilter filterString: 'as') narrowKey: 'as'.
	fetcher next: 10.
	fetcher := fetcher narrowFilter: (CoCaseSensitiveBeginsWithFilter filterString: 'asL') narrowKey: 'asL'.

	narrowedResults := fetcher next: 10.

	self assertCollection: originalSearch equals: narrowedResults
]

{ #category : 'tests' }
CoNarrowHistoryFetcherTest >> testUnnarrowingAndThenNarrowingReturnsSameResult [


	| originalResults newResults |
	fetcher := (CoCollectionFetcher onCollection: #((
		as1
		asCol
		as2
		asCC
		as3
		asCCC
	) collect: [ :s | #CoSelectorEntry new contents: s ])) withNarrowHistory.
	fetcher := fetcher
		           narrowFilter: (CoCaseSensitiveBeginsWithFilter filterString: 'as')
		           narrowKey: 'as'.
	fetcher upToEnd.

	fetcher := fetcher
		           narrowFilter: (CoCaseSensitiveBeginsWithFilter filterString: 'asL')
		           narrowKey: 'asC'.
	originalResults := fetcher upToEnd.

	fetcher := fetcher
		      unnarrowFilter: (CoCaseSensitiveBeginsWithFilter filterString: 'as')
		      narrowKey: 'as'.
	fetcher upToEnd.

	fetcher := fetcher
		           narrowFilter: (CoCaseSensitiveBeginsWithFilter filterString: 'asL')
		           narrowKey: 'asC'.
	newResults := fetcher upToEnd.

	self assertCollection: originalResults equals: newResults
]
