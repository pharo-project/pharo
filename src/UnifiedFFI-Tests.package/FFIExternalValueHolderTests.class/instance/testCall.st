tests
testCall
	| object time |
	
	TIME_T_PTR := FFIExternalValueHolder ofType: 'uint32'.
	object := TIME_T_PTR new.

	time := self getTime: object.
	self assert: time equals: object value.