Extension { #name : #SequenceableCollection }

{ #category : #'*Deprecated80' }
SequenceableCollection >> concatenation [
	self deprecated: 'Use flattened instead' transformWith: '`@receiver concatenation' ->'`@receiver flattened'.
	^ self flattened
]
