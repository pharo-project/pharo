api-events
whenBorderWidthChanged: aBlock
	"Set a block to be performed when the brder width changed"
	
	borderWidth whenChangedDo: aBlock