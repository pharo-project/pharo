stepping
step

	owner ifNil: [ ^ self stopStepping ].
	self updateContents.
	self changed