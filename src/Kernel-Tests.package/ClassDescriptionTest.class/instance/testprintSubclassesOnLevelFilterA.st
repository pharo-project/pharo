tests - slots
testprintSubclassesOnLevelFilterA
	| expected result stream |
	expected := '
	ExampleForTest1 #()
		ExampleForTest11 #()
			ExampleForTest112 #()'.
	result := String new: expected size.
	stream := ReadWriteStream on: result.
	ExampleForTest1 printSubclassesOn: stream level: 1 filter: {ExampleForTest1. ExampleForTest11. ExampleForTest112}.
	self assert: result equals: expected