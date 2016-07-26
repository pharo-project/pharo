scrollpane-calculations
calculateVelocity: event
	|delta|
	delta := event timeStamp - lastScrollTimeStamp.
		
	self velocity: (delta > self class defaultScrollTimeReset
		ifTrue: [ self class defaultScrollVelocity ]
		ifFalse: [ self velocityValue: delta ]).
		
	lastScrollTimeStamp := event timeStamp.
	
	^ self velocity ceiling