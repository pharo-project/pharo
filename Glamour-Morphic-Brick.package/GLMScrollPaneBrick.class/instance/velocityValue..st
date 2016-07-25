scrollpane-calculations
velocityValue: anInteger
	|time|
	
	time := anInteger <= self class defaultScrollTurboTime
		ifTrue: [ 2 ]
		ifFalse: [ 1 ].
	^ (self velocity + (self class defaultScrollAcceleration * time)) min: self calculateMaxVelocity