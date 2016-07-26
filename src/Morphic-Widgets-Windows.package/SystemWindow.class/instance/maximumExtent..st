initialization
maximumExtent: aPoint
	"This returns the maximum extent that the morph may be expanded to.
	Return nil if this property has not been set."

	^ self setProperty: #maximumExtent toValue: aPoint