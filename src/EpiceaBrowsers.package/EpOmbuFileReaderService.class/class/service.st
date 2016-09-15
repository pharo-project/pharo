System-FileRegistry
service
	^ SimpleServiceEntry 
		provider: self 
		label: 'Browse Epicea Log'
		selector: #browseLogInFileNamed:
		description: 'Browse the Epicea log.'
		buttonLabel: 'Browse Epicea Log'