tests
testNewWithSize
	|array|
	array := Array new: 5.
	self assert: array size = 5.
	1 to: 5 do: [:index | self assert: (array at: index) isNil]