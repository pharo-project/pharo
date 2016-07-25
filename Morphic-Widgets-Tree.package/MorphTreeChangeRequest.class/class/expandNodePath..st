instance creation
expandNodePath: aNodePath
	^ self new action: #expandNodePath:; arguments: (Array with: aNodePath)