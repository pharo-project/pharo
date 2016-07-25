real tests
differentExceptionInShouldRaiseWithExceptionDoTest

	[ self 
		should: [ Error signal ]
		raise: Halt
		withExceptionDo: [ :anException | self assert: false description: 'should:raise:withExceptionDo: handled an exception that should not handle'] ]
	on: Error
	do: [ :anException | anException return: nil ]