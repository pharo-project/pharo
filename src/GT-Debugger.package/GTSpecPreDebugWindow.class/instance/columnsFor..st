printing
columnsFor: aContext

	^ { 
		self debugger formatStackClassColumnForContext: aContext.
		self debugger formatStackMethodColumnForContext: aContext.
		self debugger formatStackExtraColumnForContext: aContext  }