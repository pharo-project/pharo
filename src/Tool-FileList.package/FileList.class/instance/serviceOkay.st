own services
serviceOkay
	"Answer a service for hitting the okay button"

	^ (SimpleServiceEntry new
		provider: self 
		label: 'Okay' 
		selector: #okHit 
		description: 'Hit here to accept the current selection')
		buttonLabel: 'Ok'