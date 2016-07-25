transforming
align: alignmentPoint with: relativePoint 
	"Translate the receiver's offset such that alignmentPoint aligns with 
	relativePoint."

	self offset: (self offset translateBy: relativePoint - alignmentPoint)