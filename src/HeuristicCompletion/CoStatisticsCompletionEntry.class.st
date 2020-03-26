"
I am an entry to put in the list of candidates in the code completion list.
I wrap a normal entry (necEntry) and logs acceptance when I'm sent the #activateOn: message.

I'm meant to be created by a CoStatisticsHeuristicWrapper for each completion entry.
See CoStatisticsHeuristicWrapper for more details.
"
Class {
	#name : #CoStatisticsCompletionEntry,
	#superclass : #Object,
	#instVars : [
		'necEntry',
		'index',
		'heuristic',
		'statistics'
	],
	#category : #'HeuristicCompletion-Statistics'
}
