"
I represent the event of having accepted entry in the candidate list of the code completion.
I have the index of the entry and the source that produced it (typically the heuristic)
"
Class {
	#name : #CoEntryAcceptedEvent,
	#superclass : #CoUsageEvent,
	#instVars : [
		'completedEntry',
		'source',
		'index'
	],
	#category : #'HeuristicCompletion-Statistics'
}
