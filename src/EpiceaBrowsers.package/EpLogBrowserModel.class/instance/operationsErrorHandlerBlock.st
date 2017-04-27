menu - operations
operationsErrorHandlerBlock

	^ [ :error | | message |
		message := self title, ' error on operation: ', error asString.
		message logCr.
		self inform: message. ]