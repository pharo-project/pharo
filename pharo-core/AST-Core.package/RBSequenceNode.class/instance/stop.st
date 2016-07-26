accessing
stop
	^(periods isEmpty ifTrue: [0] ifFalse: [periods last]) 
		max: (statements isEmpty ifTrue: [0] ifFalse: [statements last stop])