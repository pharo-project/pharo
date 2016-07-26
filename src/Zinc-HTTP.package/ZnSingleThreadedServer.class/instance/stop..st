public
stop: unregister
	"Stop me. I will stop listening on my port for incoming HTTP connections.
	If unregister is true, unregister me from the list of managed instances.
	Does nothing when I am not running"
	
	self isRunning ifFalse: [ ^ self ].
	process terminate. 
	process := nil.
	unregister ifTrue: [ self unregister ].
	self closeDelegate.
	self logStopped