public api
shouldUseCache: aBoolean
	^ self parameters 
		at: 'shouldUseCache'
		put: aBoolean