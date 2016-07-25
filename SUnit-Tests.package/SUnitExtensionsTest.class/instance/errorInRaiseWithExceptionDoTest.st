real tests
errorInRaiseWithExceptionDoTest

	self 
		should: [ Error  signal ]
		raise: Error
		withExceptionDo: [ :anException | Error signal: 'A forced error' ]