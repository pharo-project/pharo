accessing
exceptionHandler
	^ exceptionHandler ifNil: [ exceptionHandler := self defaultExceptionHandler ]