private
shouldntRaise: anError process: aString origin: anOrigin during: exceptionHandler
	spotter exceptionHandler: exceptionHandler.
	self assert: spotter exceptionHandler == exceptionHandler.
	
	self shouldnt: [ self process: aString origin: anOrigin ] raise: anError.
	
	self assert: spotter exceptionHandler == exceptionHandler.