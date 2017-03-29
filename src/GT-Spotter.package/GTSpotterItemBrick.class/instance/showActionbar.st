actions
showActionbar
	self shouldShowActionbar
		ifFalse: [ ^ self ].

	self actionbarBrick visible: true