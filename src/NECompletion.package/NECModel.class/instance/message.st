accessing
message
	^self isEmpty 
		ifTrue: ['no completions found'] 
		ifFalse: [nil]