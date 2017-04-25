source
renameTextSelectionForMethod
	| selectedInterval  method tempNode |
	method := self model selectedMethod.
	selectedInterval := self model selectionInterval.
	selectedInterval first = 1
		ifTrue: [ ^ self renameMethodFor: method ].
	tempNode := (method ast bestNodeFor: selectedInterval).
	self renameTemporaryNamed: tempNode asString Between: (tempNode start to: tempNode stop) from: method