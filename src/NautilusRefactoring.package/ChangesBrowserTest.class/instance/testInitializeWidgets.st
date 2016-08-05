tests
testInitializeWidgets
	self assert: browser changesTree isNotNil.
	self assert: browser textArea isNotNil.
	self
		assertCollection: browser focusOrder
		equals:
			{browser changesTree.
			browser textArea} asOrderedCollection