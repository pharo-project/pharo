initializing
initialize
	self flag: 'error handling'.
	
	self pages: self defaultPages.
	self files do: [ :each | 
		[ self pages add: (GTPlayPage fromFileReference: each) ] " bypass "
			on: Error 
			do: [ " unhandled exceptions are bad design - we should handle it somehow " ] ]