instance creation
expandItemPath: anItemPath
	^ self new action: #expandItemPath:; arguments: (Array with: anItemPath)