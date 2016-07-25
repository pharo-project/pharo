operations
mergeUpTo: aSpan
	aSpan = self next ifTrue: [ ^ self next ].
	^ self next mergeUpTo: aSpan.