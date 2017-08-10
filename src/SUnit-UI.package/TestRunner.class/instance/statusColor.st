accessing-testing
statusColor
	result hasErrors 
		ifTrue: [ ^ self theme dangerBackgroundColor ].
	result hasFailures 
		ifTrue: [ ^ self theme warningBackgroundColor ].
	^ self theme successBackgroundColor