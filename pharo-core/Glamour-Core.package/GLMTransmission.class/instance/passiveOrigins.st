accessing
passiveOrigins 
	 
	^(self originReferences select: [:each | each isPassive]) 
		collect: [:each | each port]