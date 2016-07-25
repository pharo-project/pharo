own services
serviceCopyName

	^ (SimpleServiceEntry 
		provider: self 
		label: 'Copy name to clipboard' 
		selector: #copyName 
		description: 'Copy name to clipboard' )