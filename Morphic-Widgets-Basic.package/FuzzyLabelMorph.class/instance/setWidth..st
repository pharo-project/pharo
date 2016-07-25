as yet unclassified
setWidth: width
	"Set the width/extent."
	
	self extent: width @ (self fontToUse height ceiling + (2 * self offset))