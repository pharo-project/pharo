tests
testToSend
	| list |
	list := GLMListPresentation new.
	list selectionTransformation: [ :x | x + 1 ].
	list entity: (1 to: 100).
	list selection: 41.
	self assert: list selection equals: 42