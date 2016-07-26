instance creation
origin: originPoint extent: extentPoint 
	"Answer an instance of me whose top left corner is originPoint and width by height is extentPoint. Note that extentPoint should be non negative since it represents the size of the rectangle"

	
	^self basicNew 
		setPoint: originPoint 
		point: (originPoint x+ (extentPoint x max: 0)) @ (originPoint y + (extentPoint y max: 0)).