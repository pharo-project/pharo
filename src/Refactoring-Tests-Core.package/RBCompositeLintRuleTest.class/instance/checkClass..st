accessing
checkClass: aSmalllintContext 
	rules do: 
			[:each | 
			each checkClass: aSmalllintContext.
			Processor yield]