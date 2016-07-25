event handling
tabAmongFields
	^ self theme settings tabAmongFields
		or: [self hasProperty: #tabAmongFields] 