tests
testCall
	| object time |
	
	object := FFIConstantHandle new.
	time := self getTime: object.
	self assert: time equals: object handle.