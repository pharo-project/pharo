"
I am an object keeping code completion statistics.
I store two collection of events: accepted events and cancelled events.
Both events contain information about the completion context (class, is workspace...).
Accepted events also contain the text accepted, the index, and the heuristic.

I provide methods to access and query the statistics.

I have a singleton instance for convenience.
"
Class {
	#name : #CoStatistics,
	#superclass : #Object,
	#instVars : [
		'acceptedEntries',
		'cancelledCompletions'
	],
	#classVars : [
		'UniqueInstance'
	],
	#category : #'HeuristicCompletion-Statistics'
}
