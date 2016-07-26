halos and balloon help
balloonFont
	^ self
		valueOfProperty: #balloonFont
		ifAbsent: [self defaultBalloonFont]