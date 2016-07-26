debugging actions
terminate
	"Action that needs to be executed after the window containing this debug session is closed, 
	in order to terminate the right process."
	
	self interruptedProcess 
		ifNotNil: [ 
			"Assume the user closed the debugger. Simply kill the interrupted process."
			self interruptedProcess terminate.
			self clear.
			Smalltalk installLowSpaceWatcher.  "restart low space handler" ]
		ifNil: [ 
			"Assume the interrupted process was resumed."
			"Kill the active process if the error was in the UI as there should be only one UI process."
			self isAboutUIProcess
				ifTrue: [Processor terminateActive] ]
	