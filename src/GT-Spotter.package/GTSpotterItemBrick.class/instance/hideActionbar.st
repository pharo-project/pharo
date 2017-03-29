actions
hideActionbar

	self shouldShowActionbar
		ifTrue: [ ^ self ].

	self actionbarBrick visible: false