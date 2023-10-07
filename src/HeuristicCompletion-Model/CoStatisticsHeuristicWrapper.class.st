"
I am an heuristic decorator that wraps an heuristic.
When asked to build a fetcher, I build the fetcher of the wrapped heuristic, and decorate it with a mapping fetcher. This mapping fetcher wraps the completion entries returned by the fetcher with a logging entry.

I'm created by the CoStatisticsHeuristicBuilder. See CoStatisticsHeuristicBuilder for more details.
"
Class {
	#name : 'CoStatisticsHeuristicWrapper',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#instVars : [
		'wrapped',
		'statistics'
	],
	#category : 'HeuristicCompletion-Model-Statistics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Statistics'
}

{ #category : 'requests' }
CoStatisticsHeuristicWrapper >> appliesForNode: aNode inContext: aContext [

	^ wrapped appliesForNode: aNode inContext: aContext
]

{ #category : 'requests' }
CoStatisticsHeuristicWrapper >> buildFetcherFor: aNode inContext: aContext [

	| fetcher wrappedFetcher |
	fetcher := wrapped buildFetcherFor: aNode inContext: aContext.
	wrappedFetcher := fetcher collect: [ :e |
		(CoStatisticsCompletionEntry on: e)
			heuristic: self next;
			statistics: statistics;
			yourself ].
	^ wrappedFetcher
]

{ #category : 'testing' }
CoStatisticsHeuristicWrapper >> isStatisticsHeuristicWrapper [

	^ true
]

{ #category : 'accessing' }
CoStatisticsHeuristicWrapper >> statistics [
	^ statistics
]

{ #category : 'accessing' }
CoStatisticsHeuristicWrapper >> statistics: anObject [
	statistics := anObject
]

{ #category : 'accessing' }
CoStatisticsHeuristicWrapper >> wrapped [
	^ wrapped
]

{ #category : 'accessing' }
CoStatisticsHeuristicWrapper >> wrapped: anObject [
	wrapped := anObject
]
