Extension { #name : #SequenceableCollection }

{ #category : #'*Collections-arithmetic' }
SequenceableCollection >> running: aBlock of: aSubsetSize [
	"This is a generalization of a running average (a.k.a. moving average, rolling average) which allows you to apply any given block to the shifting subsets of a given size.
	
	For example, given a collection #(1 2 3 4 5) and a window size 2, we collect subsets of this collection by starting with first 2 elements and shifting the window 1 element to the right: #((1 2)(2 3)(3 4)(4 5)), then we apply aBlock to each subset and collect the results. For example, if aBlock is [ :subset | subset average ], this will give us #(1.5 2.5 3.5 4.5)"
	| result |
	
	aSubsetSize > self size ifTrue: [
		SubscriptOutOfBounds
			signal: 'The subset size can not exceed the size of a collection' ].
		
	aSubsetSize < 0 ifTrue: [
		SubscriptOutOfBounds
			signal: 'The subset size must be positive' ].
	
	result := (1 to: self size - aSubsetSize + 1) collect: [ :i |
		aBlock value: (self copyFrom: i to: i + aSubsetSize - 1) ].
	
	^ self species withAll: result
]

{ #category : #'*Collections-arithmetic' }
SequenceableCollection >> runningAverage: aSubsetSize [
	"Running average (a.k.a. moving average, rolling average). See the comment of self >> #running:of: for more information."
	"(#(1 1 2 2 3 3) runningAverage: 2) >>> {1 . (3/2) . 2 . (5/2) . 3}"

	^ self running: [ :subset | subset average ] of: aSubsetSize
]

{ #category : #'*Collections-arithmetic' }
SequenceableCollection >> runningMax: aSubsetSize [
	"Running max. See the comment of self >> #running:of: for more information."
	"(#(1 1 2 2 3 3) runningMax: 3) >>> {2 . 2 . 3 . 3}"

	^ self running: [ :subset | subset max ] of: aSubsetSize
]

{ #category : #'*Collections-arithmetic' }
SequenceableCollection >> runningMin: aSubsetSize [
	"Running min. See the comment of self >> #running:of: for more information."
	"(#(1 1 2 2 3 3) runningMin: 3) >>> {1 . 1 . 2 . 2}"

	^ self running: [ :subset | subset min ] of: aSubsetSize
]
