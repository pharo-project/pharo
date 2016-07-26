accessing text
textColor
	^ textColor ifNil: [ textColor := self defaultTextColor ]