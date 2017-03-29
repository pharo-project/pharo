accessing - dynamic
exceptionHandler
	^ GTCurrentSpotterExceptionHandler value ifNil: [ self spotter exceptionHandler ]
	" #ifNil: is necessary because the sender chain of #spotterProcessorsFor:pragma: may have multiple callees outside of the exception handler (async) ... so we have to provide a global but yet easy fallback "