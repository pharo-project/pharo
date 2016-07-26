real tests
assertionFailedInRaiseWithExceptionDoTest

	self 
		should: [ Error signal ]
		raise: Error
		withExceptionDo: [ :anException | self assert: false ]