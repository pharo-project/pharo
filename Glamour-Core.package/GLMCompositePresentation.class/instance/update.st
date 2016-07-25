updating
update
	self pane ports do: [:each |
		(self validate: each value on: each) ifFalse: [each resetValue] ].
	self presentations do: [ :each | each update ].
	self announce: (GLMPresentationUpdated new presentation: self).