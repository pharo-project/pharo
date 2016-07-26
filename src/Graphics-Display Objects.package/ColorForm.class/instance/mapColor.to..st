color manipulation
mapColor: oldColor to: newColor
	"Replace all occurances of the given color with the given new color in my color map."

	self ensureColorArrayExists.
	1 to: colors size do: [:i | 
		(colors at: i) = oldColor ifTrue: [colors at: i put: newColor]].
	self clearColormapCache.
