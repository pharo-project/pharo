tests - window creation and deletion
testWindowCreation
	| t oldBounds newBounds |
	t := 0.
	World announcer when: WindowResizing do: [:ann | t := t + 1].
	window := SystemWindow labelled: 'foo'.
	window setProperty: #minimumExtent toValue: 1@1.
	window openInWorld.
	oldBounds := window bounds.
	window announcer when: WindowResizing do: [:ann | t := t + 1].
	self assert: (t = 0).
	window extent: 50@60.
	newBounds := window bounds.
	self assert: (t = 1).
