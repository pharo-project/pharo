instance creation
origin: originPoint corner: cornerPoint 
	"Answer an instance of me whose corners (top left and bottom right) are 
	determined by the arguments."

	^self basicNew setPoint: originPoint point: cornerPoint