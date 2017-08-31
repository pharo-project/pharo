private
handleError: error

	self debugMode 
		ifTrue: [ error signal ]
		ifFalse: [ 'EpMonitor could not log a change. You can enable debugging with "EpMonitor current debugMode: true".' logCr ]