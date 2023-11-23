"
I am an entry to put in the list of candidates in the code completion list.
I wrap a normal entry (necEntry) and logs acceptance when I'm sent the #activateOn: message.

I'm meant to be created by a CoStatisticsHeuristicWrapper for each completion entry.
See CoStatisticsHeuristicWrapper for more details
"
Class {
	#name : 'CoStatisticsCompletionEntry',
	#superclass : 'Object',
	#instVars : [
		'necEntry',
		'index',
		'heuristic',
		'statistics'
	],
	#category : 'HeuristicCompletion-Model-Statistics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Statistics'
}

{ #category : 'instance creation' }
CoStatisticsCompletionEntry class >> on: aNECEntry [

	^ self new
		necEntry: aNECEntry;
		yourself
]

{ #category : 'comparing' }
CoStatisticsCompletionEntry >> = anEntry [

	^ self contents = anEntry contents
]

{ #category : 'activation' }
CoStatisticsCompletionEntry >> activateOn: aCoCompletionContext [

	| entry |
	entry := CoEntryAcceptedEvent new
		index: index;
		source: heuristic;
		typedToken: aCoCompletionContext completionToken;
		completedEntry: self contents.
	statistics logAcceptEvent: entry.
	necEntry activateOn: aCoCompletionContext
]

{ #category : 'accessing' }
CoStatisticsCompletionEntry >> contents [

	^ necEntry contents
]

{ #category : 'comparing' }
CoStatisticsCompletionEntry >> hash [

	^ self contents hash
]

{ #category : 'accessing' }
CoStatisticsCompletionEntry >> heuristic [
	^ heuristic
]

{ #category : 'accessing' }
CoStatisticsCompletionEntry >> heuristic: anObject [
	heuristic := anObject
]

{ #category : 'accessing' }
CoStatisticsCompletionEntry >> hightlightSymbol [

	^ necEntry hightlightSymbol
]

{ #category : 'accessing' }
CoStatisticsCompletionEntry >> index [
	^ index
]

{ #category : 'accessing' }
CoStatisticsCompletionEntry >> index: anInteger [

	index := anInteger
]

{ #category : 'accessing' }
CoStatisticsCompletionEntry >> necEntry [
	^ necEntry
]

{ #category : 'accessing' }
CoStatisticsCompletionEntry >> necEntry: aNECEntry [
	"self haltIf: [ aNECEntry isKindOf: self class ]."
	necEntry := aNECEntry
]

{ #category : 'accessing' }
CoStatisticsCompletionEntry >> statistics [
	^ statistics
]

{ #category : 'accessing' }
CoStatisticsCompletionEntry >> statistics: anObject [
	statistics := anObject
]
