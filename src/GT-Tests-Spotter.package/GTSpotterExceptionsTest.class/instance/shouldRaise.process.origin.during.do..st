private
shouldRaise: anErrorClass process: aString origin: anOrigin during: exceptionHandler do: aBlock
	self assertResetExceptions.

	spotter exceptionHandler: exceptionHandler.
	self assert: spotter exceptionHandler == exceptionHandler.
	
	self should: [ self process: aString origin: anOrigin ] 
		raise: anErrorClass 
		withExceptionDo: [ :exception | 
			self assert: exception class == anErrorClass.
			aBlock value: exception. ].
		
	self assert: spotter exceptionHandler == exceptionHandler.
	
	self assertException: anErrorClass