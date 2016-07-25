default actions
lowSpaceWatcherDefaultAction: preemptedProcess
	| ctx |
	"yeah.. space is low.. quit!! :)"
	
	ctx := preemptedProcess ifNotNil: [  preemptedProcess suspendedContext ].
	ctx ifNil: [ ctx := thisContext sender ]. 
	self quitFrom: ctx withMessage: 'Low space signal received.'.
	
	self error. "what else we can do? "