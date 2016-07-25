accessing
underlineColor
	^ underlineColor ifNil: [ underlineColor := self defaultUnderlineColor ]