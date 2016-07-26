own services
serviceViewContentsInWorkspace
	"Answer a service for viewing the contents of a file in a workspace"
	
	^ (SimpleServiceEntry 
		provider: self 
		label: 'Workspace with contents' 
		selector: #viewContentsInWorkspace)
		description: 'Open a new Workspace whose contents are set to the contents of this file'