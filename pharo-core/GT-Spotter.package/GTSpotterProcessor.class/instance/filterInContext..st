private
filterInContext: aSpotterContext
	[ self basicFilterInContext: aSpotterContext ]
		on: Error 
		do: [ :exception | 
			self exceptionHandler 
				handleException: exception 
				do: [ ('[Spotter] Exception in processor <', self title asString, '>: ', exception asString) logCr ] ]