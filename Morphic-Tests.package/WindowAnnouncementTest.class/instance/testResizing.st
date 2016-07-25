tests - window change
testResizing
	| t oldBounds newBounds |
	window := SystemWindow labelled: 'foo'.
	t := 0.
	window openInWorld.
	oldBounds := window bounds.
	window announcer when: WindowResizing do: [:ann | t := t + 1].
	self assert: (t = 0).
	window extent: 50@60.
	newBounds := window bounds.
	self assert: (t = 1).
