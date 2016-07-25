public
handleException: anException
	^ self handleException: anException do: [ 
		('[Spotter] Exception: ', anException asString) logCr ]