own services
serviceViewContents
	"Answer a service for viewing the contents of a file in a workspace"
	
	^ (SimpleServiceEntry 
		provider: self 
		label: 'Open contents' 
		selector: #viewContents)
		description: 'Open a text window whose contents are set to the contents of this file'