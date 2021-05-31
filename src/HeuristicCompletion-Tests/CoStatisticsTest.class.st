Class {
	#name : #CoStatisticsTest,
	#superclass : #CoBasicCompletionTest,
	#instVars : [
		'statistics'
	],
	#category : #'HeuristicCompletion-Tests-Statistics'
}

{ #category : #'tests - average' }
CoStatisticsTest >> logAcceptedEvent: anIndex [

	self logAcceptedEvent: anIndex typedToken: 'to'
]

{ #category : #'tests - average' }
CoStatisticsTest >> logAcceptedEvent: anIndex typedToken: aToken [

	| entry |
	entry := CoEntryAcceptedEvent new
		index: anIndex;
		source: 'heuristic';
		typedToken: aToken;
		completedEntry: 'toto'.
	
	statistics logAcceptEvent: entry
]

{ #category : #'tests - average' }
CoStatisticsTest >> testAcceptEntryIncrementsAverage [
	statistics := CoStatistics new.
	self logAcceptedEvent: 1.
	self logAcceptedEvent: 1 typedToken: 'tototo'.
	self assert: statistics averageTypedCharacters equals: 4
]

{ #category : #'tests - average' }
CoStatisticsTest >> testAcceptEntryIncrementsAverageEvenIfAtDifferentIndexes [
	statistics := CoStatistics new.
	self logAcceptedEvent: 1.
	self logAcceptedEvent: 2 typedToken: 'tototo'.
	self assert: statistics averageTypedCharacters equals: 4
]

{ #category : #'tests - entries' }
CoStatisticsTest >> testAcceptEntryLogsAllSources [

	statistics := CoStatistics new.
	self logAcceptedEvent: 1.
	self logAcceptedEvent: 2.
	self assert: (statistics entriesGeneratedBy: 'heuristic') equals: 2
]

{ #category : #'tests - average' }
CoStatisticsTest >> testAcceptEntryLogsAverageTypedCharacters [
	statistics := CoStatistics new.
	self logAcceptedEvent: 1.
	self assert: statistics averageTypedCharacters equals: 2
]

{ #category : #'tests - average' }
CoStatisticsTest >> testAcceptEntryLogsAverageTypedCharactersAtPosition [
	statistics := CoStatistics new.
	self logAcceptedEvent: 1.
	self assert: (statistics averageTypedCharactersAt: 1) equals: 2
]

{ #category : #'tests - average' }
CoStatisticsTest >> testAcceptEntryLogsAverageTypedCharactersDoesNotAffectOtherIndexes [
	statistics := CoStatistics new.
	self logAcceptedEvent: 2.
	self assert: (statistics averageTypedCharactersAt: 1) equals: 0
]

{ #category : #'tests - entries' }
CoStatisticsTest >> testAcceptEntryLogsIt [
	statistics := CoStatistics new.
	self logAcceptedEvent: 1.
	self assert: statistics acceptedEntries notEmpty
]

{ #category : #'tests - entries' }
CoStatisticsTest >> testAcceptEntryLogsItsSize [
	statistics := CoStatistics new.
	self logAcceptedEvent: 1.
	self assert: (statistics entriesAtIndex: 1) size equals: 1
]

{ #category : #'tests - entries' }
CoStatisticsTest >> testAcceptEntryLogsItsSource [

	| entries |
	statistics := CoStatistics new.
	self logAcceptedEvent: 1.
	
	entries := statistics groupBy: [ :e | e index = 1 and: [ e source = 'heuristic' ] ].
	
	self assert: entries size equals: 1
]

{ #category : #'tests - entries' }
CoStatisticsTest >> testAcceptEntryLogsItsSourceInSeparateEntries [

	| entries |
	statistics := CoStatistics new.
	self logAcceptedEvent: 2.
	entries := statistics groupBy: [ :e | e index = 1 and: [ e source = 'heuristic' ] ].
	self assert: entries size equals: 0
]

{ #category : #'tests - entries' }
CoStatisticsTest >> testAcceptTwoEntriesAtDifferentIndexLogsSeparateCounts [
	statistics := CoStatistics new.
	self logAcceptedEvent: 1.
	self logAcceptedEvent: 2.
	self assert: (statistics entriesAtIndex: 1) size equals: 1.
	self assert: (statistics entriesAtIndex: 2) size equals: 1
]

{ #category : #'tests - entries' }
CoStatisticsTest >> testAcceptTwoEntriesAtSameIndexLogsCount [
	statistics := CoStatistics new.
	self logAcceptedEvent: 1.
	self logAcceptedEvent: 1.
	self assert: (statistics entriesAtIndex: 1) size equals: 2
]

{ #category : #'tests - closing' }
CoStatisticsTest >> testCancelCompletionInClassWithDynamicExtentDoesNotLogOtherClass [

	statistics := CoStatistics new.
	statistics logCancelCompletionFromTypedToken: 'to' inClass: #Object withDynamicExtent: false.
	self assert: (statistics numberOfCancelsByClass: #Point) equals: 0
]

{ #category : #'tests - closing' }
CoStatisticsTest >> testCancelCompletionInClassWithDynamicExtentLogsIt [

	statistics := CoStatistics new.
	statistics logCancelCompletionFromTypedToken: 'to' inClass: #Object withDynamicExtent: false.
	self assert: statistics numberOfCancels equals: 1
]

{ #category : #'tests - closing' }
CoStatisticsTest >> testCancelCompletionInClassWithDynamicExtentLogsPerClass [

	statistics := CoStatistics new.
	statistics logCancelCompletionFromTypedToken: 'to' inClass: #Object withDynamicExtent: false.
	self assert: (statistics numberOfCancelsByClass: #Object) equals: 1
]

{ #category : #'tests - heuristicbuilder' }
CoStatisticsTest >> testHeuristicBuilderForSingleHeuristicBuildsWrapper [

	| builder heuristic singleHeuristic |
	builder := CoStatisticsHeuristicBuilder new.
	singleHeuristic := CoGlobalVariablesHeuristic new.
	builder add: singleHeuristic.

	heuristic := builder build.
	self assert: heuristic isStatisticsHeuristicWrapper
]

{ #category : #'tests - heuristicbuilder' }
CoStatisticsTest >> testHeuristicBuilderForSingleHeuristicLastIsNullHeuristic [

	| builder heuristic singleHeuristic |
	builder := CoStatisticsHeuristicBuilder new.
	singleHeuristic := CoGlobalVariablesHeuristic new.
	builder add: singleHeuristic.

	heuristic := builder build.
	self assert: heuristic next isNullHeuristic
]

{ #category : #'tests - heuristicbuilder' }
CoStatisticsTest >> testHeuristicBuilderForSingleHeuristicWrapsNonWrapper [

	| builder heuristic singleHeuristic |
	builder := CoStatisticsHeuristicBuilder new.
	singleHeuristic := CoGlobalVariablesHeuristic new.
	builder add: singleHeuristic.

	heuristic := builder build.
	self assert: heuristic wrapped equals: singleHeuristic
]

{ #category : #'tests - heuristicbuilder' }
CoStatisticsTest >> testHeuristicBuilderForTwoHeuristicBuildsWrapper [
	| builder heuristic |
	builder := CoStatisticsHeuristicBuilder new.
	builder add: CoGlobalVariablesHeuristic new.
	builder add: CoGlobalVariablesHeuristic new.
	heuristic := builder build.

	self assert: heuristic isStatisticsHeuristicWrapper
]

{ #category : #'tests - heuristicbuilder' }
CoStatisticsTest >> testHeuristicBuilderForTwoHeuristicNextIsWrapper [
	| builder heuristic |
	builder := CoStatisticsHeuristicBuilder new.
	builder add: CoGlobalVariablesHeuristic new.
	builder add: CoGlobalVariablesHeuristic new.
	heuristic := builder build.

	self assert: heuristic next isStatisticsHeuristicWrapper
]

{ #category : #'tests - heuristicbuilder' }
CoStatisticsTest >> testHeuristicBuilderForTwoHeuristicSecondWrapperWrapsNonWrapper [
	| builder heuristic heuristic2 |
	builder := CoStatisticsHeuristicBuilder new.
	builder add: CoGlobalVariablesHeuristic new.
	builder add: (heuristic2 := CoGlobalVariablesHeuristic new).
	heuristic := builder build.

	self assert: heuristic next wrapped equals: heuristic2
]

{ #category : #'tests - heuristicbuilder' }
CoStatisticsTest >> testHeuristicBuilderForTwoHeuristicWrapsNonWrapper [
	| builder heuristic heuristic1 |
	builder := CoStatisticsHeuristicBuilder new.
	builder add: (heuristic1 := CoGlobalVariablesHeuristic new).
	builder add: CoGlobalVariablesHeuristic new.
	heuristic := builder build.

	self assert: heuristic wrapped equals: heuristic1
]

{ #category : #'tests - entries' }
CoStatisticsTest >> testNewStatisticsHaveNoAcceptedEntries [

	statistics := CoStatistics new.
	self assert: statistics acceptedEntries isEmpty
]
