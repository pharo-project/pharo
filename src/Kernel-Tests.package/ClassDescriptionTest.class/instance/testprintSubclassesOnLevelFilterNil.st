tests - slots
testprintSubclassesOnLevelFilterNil
	| expected result stream |
	expected := '
	ExampleForTest1 #()
		ExampleForTest11 #()
			ExampleForTest111 #()
			ExampleForTest112 #()
		ExampleForTest12 #()'.
	result := String new: expected size.
	stream := ReadWriteStream on: result.
	ExampleForTest1 printSubclassesOn: stream level: 1 filter: nil.
	self assert: result equals: expected