options
port: integer
	"Set the port number we will be listening on.
	Cannot be changed after the server is already running."
	
	(self isRunning and: [ self port ~= integer ]) 
		ifTrue: [ self error: 'Stop me before changing my port' ].
	self optionAt: #port put: integer