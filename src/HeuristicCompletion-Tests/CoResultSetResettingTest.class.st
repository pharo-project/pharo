Class {
	#name : 'CoResultSetResettingTest',
	#superclass : 'CoBasicCompletionTest',
	#instVars : [
		'completion',
		'mockFilter'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoResultSetResettingTest >> setUp [

	super setUp.
	completion := CoResultSet fetcher: (mockFilter := CoMockFetcherToRecordResets new)
]

{ #category : 'tests' }
CoResultSetResettingTest >> testBroadenFilterShouldResetTheFilter [

	completion replaceFilterWith: (CoCaseInsensitiveBeginsWithFilter filterString: 'BA').
	completion fetch: 1.
	completion replaceFilterWith: (CoCaseInsensitiveBeginsWithFilter filterString: 'B').

	self
		assert: mockFilter resetingCount
		equals: 1 + 1 "There is the initial one, and then the reset after setting a new one"
]

{ #category : 'tests' }
CoResultSetResettingTest >> testNarrowingFilterShouldNotResetTheFilter [

	completion replaceFilterWith: (CoCaseInsensitiveBeginsWithFilter filterString: 'BA').
	completion fetch: 1.
	completion replaceFilterWith: (CoCaseInsensitiveBeginsWithFilter filterString: 'BAC').

	self
		assert: mockFilter resetingCount
		equals: 1 "There is the initial one"
]
