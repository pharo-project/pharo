error handling
handleError: anError log: shouldLog

	shouldLog ifTrue: [ self logError: anError ].
	self debugProcess: Processor activeProcess  
		context: anError signalerContext
		label: anError description 
		fullView: false