printing/formatting
formatStackExtraColumnForContext: aContext 

	^ Text 
		string: (self printStackExtraColumnForContext: aContext )
		attributes: (self textAttributesForStackEntry: aContext) 