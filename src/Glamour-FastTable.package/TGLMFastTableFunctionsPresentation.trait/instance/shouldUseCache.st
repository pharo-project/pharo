public api
shouldUseCache
	^ self parameters 
		at: 'shouldUseCache'
		ifAbsent: [ false ]