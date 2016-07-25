real tests
noExceptionInShouldRaiseWithExceptionDoTest

	self 
		should: [  ]
		raise: Error
		withExceptionDo: [ :anException | Error signal: 'Should not get here' ]