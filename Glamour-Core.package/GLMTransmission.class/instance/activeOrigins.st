accessing
activeOrigins 
	 
	^(self originReferences select: [:each | each isActive]) 
		collect: [:each | each port]