updating
update
	self pane ports do: [:each |
		(self validate: each value on: each) ifFalse: [each resetValue] ].
	self announce: (GLMPresentationUpdated new presentation: self)