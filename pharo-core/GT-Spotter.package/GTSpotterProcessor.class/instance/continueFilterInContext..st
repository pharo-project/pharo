private
continueFilterInContext: aSpotterContext
	[ self basicContinueFilterInContext: aSpotterContext ]
		on: Error 
		do: [ :exception | 
			self exceptionHandler 
				handleException: exception 
				do: [ ('[Spotter] Exception in continuing processor <', self title asString, '>: ', exception asString) logCr ] ]