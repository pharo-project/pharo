notification
collection: aCollection do: aBlock displaying: aString

	self bypassProgressBars ifTrue: [ ^super collection: aCollection do: aBlock displaying: aString ].
	aCollection 
		do: aBlock
		displayingProgress: aString