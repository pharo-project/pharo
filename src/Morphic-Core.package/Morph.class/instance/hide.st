drawing
hide
	owner ifNil: [ ^ self ].
	self visible
		ifFalse: [ ^ self ].
	self visible: false.
	self changed