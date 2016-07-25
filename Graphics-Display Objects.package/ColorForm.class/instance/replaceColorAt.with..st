color manipulation
replaceColorAt: aPoint with: newColor
	"Replace a color map entry with newColor.  The entry replaced is the one used by aPoint.  If there are are two entries in the colorMap for the oldColor, just replace ONE!!  There are often two whites or two blacks, and this is what you want, when replacing one."

	| oldIndex |
	self ensureColorArrayExists.
	oldIndex := self pixelValueAt: aPoint.
	colors at: oldIndex+1 put: newColor.
	self clearColormapCache.
