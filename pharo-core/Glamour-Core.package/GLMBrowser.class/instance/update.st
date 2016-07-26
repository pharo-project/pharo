updating
update 
	self panes do: [:each | each update ].
	self announce: (GLMPresentationUpdated new presentation: self).