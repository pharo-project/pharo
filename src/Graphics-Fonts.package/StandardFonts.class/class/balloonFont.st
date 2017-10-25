font accessing
balloonFont
	^ BalloonFont ifNil: [BalloonFont := self defaultFont]