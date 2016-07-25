*Morphic-Widgets-Windows
fitAllVisibleWindows
	"Fit all windows as visible"
	|allowedArea|
	allowedArea := RealEstateAgent maximumUsableAreaInWorld: World.
	(self  windowsSatisfying: [:w | w isCollapsed not])
		reverseDo:[:w | 
			w extent: w initialExtent.  
			w bounds:((w position extent: w initialExtent) translatedAndSquishedToBeWithin: allowedArea).
		].
	self displayWorld