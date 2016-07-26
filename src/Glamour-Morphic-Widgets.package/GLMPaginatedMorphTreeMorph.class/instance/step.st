stepping
step

	self window ifNil: [ ^ self ].
	self window isActive ifFalse: [ ^ self ].
	(self wantsSteps and: [ self model shouldDoStep ]) ifFalse: [ ^ self ].
	(self window boundsInWorld containsRect: self boundsInWorld) ifFalse: [ ^ self ].
	self model updateRoots.
	self updateList