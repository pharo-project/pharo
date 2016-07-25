accessing-testing
statusColor
	result hasErrors 
		ifTrue: [ ^ Color red ].
	result hasFailures 
		ifTrue: [ ^ Color yellow ].
	^ Color green