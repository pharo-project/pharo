accessing
checkMethod: aSmalllintContext 
	rules do: 
			[:each | 
			each checkMethod: aSmalllintContext.
			Processor yield]