System-FileRegistry
serviceBrowseCode
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'Code-file browser'
		selector: #browseStream:
		description: 'Open a "file-contents browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'Code')
		argumentGetter: [ :fileList | fileList readOnlyStream ]