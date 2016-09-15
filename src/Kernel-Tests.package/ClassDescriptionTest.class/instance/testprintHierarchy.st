tests - slots
testprintHierarchy
	| expected result |
	expected := '
ProtoObject #()
	Object #()

		ExampleForTest1 #()
			ExampleForTest11 #()
				ExampleForTest111 #()
				ExampleForTest112 #()
			ExampleForTest12 #()'.
	result := ExampleForTest1 printHierarchy.
	self assert: result equals: expected