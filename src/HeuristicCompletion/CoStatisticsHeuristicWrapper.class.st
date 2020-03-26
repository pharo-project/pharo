"
I am an heuristic decorator that wraps a heuristic.
When asked to build a fetcher, I build the fetcher of the wrapped heuristic and decorate it with a mapping fetcher. This mapping fetcher wraps the completion entries returned by the fetcher with a logging entry.

I'm created by the CoStatisticsHeuristicBuilder. See CoStatisticsHeuristicBuilder for more details.
"
Class {
	#name : #CoStatisticsHeuristicWrapper,
	#superclass : #CoASTNodeFetcherHeuristic,
	#instVars : [
		'wrapped',
		'statistics'
	],
	#category : #'HeuristicCompletion-Statistics'
}
