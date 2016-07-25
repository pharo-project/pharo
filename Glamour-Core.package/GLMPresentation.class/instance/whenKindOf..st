scripting
whenKindOf: aClass 
	 
	self when: [:each | each isKindOf: aClass]