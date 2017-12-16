Extension { #name : #SequenceableCollection }

{ #category : #'*Deprecated70' }
SequenceableCollection >> aggregateRuns: aBlock [

	self
		deprecated: 'Please use #groupByRuns: instead'
		transformWith: '`@receiver aggregateRuns: `@statements' -> '`@receiver groupByRuns: `@statements'.
		
	^ self groupByRuns: aBlock
]
