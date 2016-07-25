color manipulation
replaceColorAtIndex: index with: newColor
	"Replace a color map entry with newColor."

	self ensureColorArrayExists.
	colors at: index put: newColor.
	cachedColormap == nil ifFalse:
		[cachedColormap at: index put: (newColor pixelValueForDepth: cachedDepth)]