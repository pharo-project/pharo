*Tool-FileList
serviceRemoveLineFeeds
	"Answer a service for removing linefeeds from a file"

	^ FileModifyingSimpleServiceEntry
		provider: self 
		label: 'Remove line feeds'
		selector: #removeLineFeeds:	
		description: 'Remove line feeds in file'
		buttonLabel: 'Remove lfs'