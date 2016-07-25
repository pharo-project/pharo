testing
testNextPutAll
	| data result |
	data := String new: 64 * 1024 withAll: $A.
	result := String streamContents: [ :out | ZnUtils nextPutAll: data on: out ].
	self assert: result equals: data