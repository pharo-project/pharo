settings
setPreferredExtentIfWanted: anExtent
	self rememberPreferredExtent ifTrue: [ 
		self preferredExtent: anExtent ]