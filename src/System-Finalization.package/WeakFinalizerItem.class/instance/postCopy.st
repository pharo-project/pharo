copying
postCopy
	executor hasMultipleExecutors ifTrue: [ executor := executor copy ].