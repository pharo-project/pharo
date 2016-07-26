halos and balloon help
balloonColor
	^ self
		valueOfProperty: #balloonColor
		ifAbsent: [self defaultBalloonColor]