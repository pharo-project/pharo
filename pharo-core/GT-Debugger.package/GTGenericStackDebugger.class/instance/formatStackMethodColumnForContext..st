printing/formatting
formatStackMethodColumnForContext: aContext 

	^ Text 
		string: (self printStackMethodColumnForContext: aContext )
		attributes: (self textAttributesForStackEntry: aContext) 