"
I am an heuristic builder that builds statistic-enabled fetchers.
I wrap each of my heuristics with a CoStatisticsHeuristicWrapper, and then compose these wrappers into a chain of responsibility.
For example, given a list of heuristics:

CoHeuristicBuilder new
	add: CoSelfMessageHeuristic new;
	add: CoSuperMessageHeuristic new;
	yourself.

I create the following chain:

CoStatisticsHeuristicWrapper -next-> CoStatisticsHeuristicWrapper
           |                                     |
        decoree                               decoree
           |                                     |
           v                                     v
   CoSelfMessageHeuristic             CoSuperMessageHeuristic
	
I am used also as a hook, that can be redefined in subclasses and build heuristics differently.
"
Class {
	#name : 'CoStatisticsHeuristicBuilder',
	#superclass : 'CoHeuristicBuilder',
	#instVars : [
		'statistics'
	],
	#category : 'HeuristicCompletion-Model-Statistics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Statistics'
}

{ #category : 'adding' }
CoStatisticsHeuristicBuilder >> add: anHeuristic [

	super add: (CoStatisticsHeuristicWrapper new
		statistics: statistics;
		wrapped: anHeuristic;
		yourself)
]

{ #category : 'accessing' }
CoStatisticsHeuristicBuilder >> statistics [
	^ statistics
]

{ #category : 'accessing' }
CoStatisticsHeuristicBuilder >> statistics: anObject [
	statistics := anObject
]
