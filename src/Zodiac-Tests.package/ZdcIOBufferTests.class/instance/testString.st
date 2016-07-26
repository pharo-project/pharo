testing
testString
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (String new: 10).
	($a to: $f) do: [ :each | ioBuffer nextPut: each ].
	self assert: ioBuffer contents = 'abcdef'.
	($a to: $f) do: [ :each | self assert: ioBuffer next = each ]
	
	