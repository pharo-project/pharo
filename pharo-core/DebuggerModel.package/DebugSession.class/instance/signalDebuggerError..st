logging
signalDebuggerError: anError

	self primitiveError:
							'Original error: ' , name asString
								,
									'.
	Smalltalk tools debugger error: '
								,
									([ anError description ]
										on: Error
										do: [ 'a ' , anError class printString ]) , ':' 