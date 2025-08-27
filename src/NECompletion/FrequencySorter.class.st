"
Sort a list of completions by frequency of usage
"
Class {
	#name : 'FrequencySorter',
	#superclass : 'CompletionSorter',
	#category : 'NECompletion-Sorting',
	#package : 'NECompletion',
	#tag : 'Sorting'
}

{ #category : 'sorting' }
FrequencySorter >> sortCompletionList: anOrderedCollection [

	^ anOrderedCollection
		sort: [ :a :b |
			| countA countB |
			countA := NECUsageTracker usageCountFor: a contents.
			countB := NECUsageTracker usageCountFor: b contents.
			(countA = countB)
				ifTrue: [ a contents < b contents ]
				ifFalse: [ countA > countB ]
		].
]
