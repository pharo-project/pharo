api-events
whenGhostTextChanged: aBlock
	"aBlock will be performed when ghostText changed"
	
	ghostText whenChangedDo: aBlock