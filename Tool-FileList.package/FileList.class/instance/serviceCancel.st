own services
serviceCancel
	"Answer a service for hitting the cancel button"

	^ (SimpleServiceEntry new
		provider: self label: 'Cancel' 
		selector: #cancelHit 
		description: 'Hit here to cancel ')
		buttonLabel: 'Cancel'