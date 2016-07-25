settings
textColor
	^ DefaultTextColor ifNil: [ DefaultTextColor := self theme textColor ]  