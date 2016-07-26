path commands
toAbsolute: aPoint

	^ absolute ifTrue: [ aPoint ] ifFalse: [ endPoint + aPoint ]