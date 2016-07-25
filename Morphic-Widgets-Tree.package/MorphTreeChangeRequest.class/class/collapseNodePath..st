instance creation
collapseNodePath: aNodePath
	^ self new action: #collapseNodePath:; arguments: (Array with: aNodePath) 