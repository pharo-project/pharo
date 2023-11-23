"
I represent the event of having accepted an entry in the candidate list of the code completion.
I have the index of the entry and the source that produced it (typically the heuristic)
"
Class {
	#name : 'CoEntryAcceptedEvent',
	#superclass : 'CoUsageEvent',
	#instVars : [
		'completedEntry',
		'source',
		'index'
	],
	#category : 'HeuristicCompletion-Model-Statistics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Statistics'
}

{ #category : 'accessing' }
CoEntryAcceptedEvent >> completedEntry [
	^ completedEntry
]

{ #category : 'accessing' }
CoEntryAcceptedEvent >> completedEntry: anObject [
	completedEntry := anObject
]

{ #category : 'accessing' }
CoEntryAcceptedEvent >> index [
	^ index
]

{ #category : 'accessing' }
CoEntryAcceptedEvent >> index: anObject [
	index := anObject
]

{ #category : 'accessing' }
CoEntryAcceptedEvent >> source [
	^ source
]

{ #category : 'accessing' }
CoEntryAcceptedEvent >> source: anObject [
	source := anObject
]
