tests
testReturn
	| object time |
	
	TIME_T_PTR := FFIExternalValueHolder ofType: 'long'.
	
	object := TIME_T_PTR new value: 0.
	time := self getTimeReturnConstantHandle: object.
	self assert: time handle equals: object value.