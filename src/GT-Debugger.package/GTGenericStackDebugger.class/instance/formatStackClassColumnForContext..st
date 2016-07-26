printing/formatting
formatStackClassColumnForContext: aContext

	^ Text 
		string: (self printStackClassColumnForContext: aContext )
		attributes: (self textAttributesForStackEntry: aContext) 
	 