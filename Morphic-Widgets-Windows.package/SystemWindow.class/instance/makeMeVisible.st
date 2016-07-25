drawing
makeMeVisible 

	self world extent > (0@0) ifFalse: [^ self].

	((self world bounds insetBy: (0@0 corner: self labelHeight asPoint))
		containsPoint: self position) ifTrue: [^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self isCollapsed
		ifTrue: [self position: (RealEstateAgent assignCollapsePointFor: self)]
		ifFalse: [self position: (RealEstateAgent initialFrameFor: self initialExtent: self extent world: self world) topLeft].

