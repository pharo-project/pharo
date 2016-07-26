events
onPrimitiveError: aString	

	" log error and quit "
	^ self quitFrom: thisContext sender withMessage: aString