menu actions
fullscreenBounds
	"Answer the bounds that the receiver would take if expanded to fullscreen."
	
	^ (RealEstateAgent maximumUsableAreaInWorld: self world) insetBy: self fullscreenMargin 
