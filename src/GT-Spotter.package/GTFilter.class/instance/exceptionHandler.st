private-model
exceptionHandler
	self flag: 'Bloc -> overrides doesNotUnderstand discarding the current exception context'.
	^ GTCurrentSpotterExceptionHandler value ifNil: [ self context exceptionHandler ]