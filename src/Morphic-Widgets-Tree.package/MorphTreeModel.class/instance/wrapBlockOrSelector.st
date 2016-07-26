accessing
wrapBlockOrSelector

	^ wrapBlockOrSelector ifNil: [wrapBlockOrSelector := #printString]