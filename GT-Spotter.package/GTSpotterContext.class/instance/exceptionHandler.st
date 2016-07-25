public
exceptionHandler
	self flag: 'Bloc -> overrides doesNotUnderstand discarding the current exception context'.
	^ GTCurrentSpotterExceptionHandler value ifNil: [ self step exceptionHandler ]