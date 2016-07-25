initialization
setController: aECController position: aPoint
	controller := aECController.
	context := controller context.
	self position: aPoint - (20 @ 0).
	self narrowCompletion
		ifFalse: [ ^ self ].
	self createTitle.
	self openInWorld