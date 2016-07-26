tests - window change
testCollapsing
	| t |
	window := SystemWindow labelled: 'foo'.
	t := 0.
	window openInWorld.
	window announcer when: WindowCollapsed do: [:ann | t := t + 1].
	self assert: (t = 0).
	window collapse.
	self assert: (t = 1).
