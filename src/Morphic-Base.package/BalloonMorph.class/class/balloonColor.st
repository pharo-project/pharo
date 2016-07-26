utility
balloonColor
	^ BalloonColor ifNil: [BalloonColor := self defaultBalloonColor]