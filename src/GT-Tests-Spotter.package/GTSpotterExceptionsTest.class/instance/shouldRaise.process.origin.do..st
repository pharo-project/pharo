private
shouldRaise: anError process: aString origin: anOrigin do: aBlock
	self shouldRaise: anError process: aString origin: anOrigin during: GTSpotterDebuggingExceptionHandlerMock new do: aBlock