accessing
backgroundColor

	^ backgroundColor ifNil: [ backgroundColor := self defaultBackgroundColor ]