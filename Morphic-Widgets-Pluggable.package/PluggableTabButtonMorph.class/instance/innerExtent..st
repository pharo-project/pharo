access
innerExtent: aPoint
	"Set the extent based on the primary visible part of the tab.  In other words add twice the cornerRadius to this extent"
	self extent: (aPoint x + (self cornerRadius * 2)) @ (aPoint y)