file support
suggestedFileName
	"If the workspace has an associated fileName use that, otherwise derive a reasonable file-name from the window label"
	
	self fileName ifNotNil: [ :f | ^ f ].
	^ window title, '.ws'