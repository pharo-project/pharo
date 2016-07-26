color manipulation
colormapIfNeededForDepth: destDepth
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	| newMap |
	colors == nil ifTrue: [
		"use the standard colormap"
		^ Color colorMapIfNeededFrom: self depth to: destDepth].

	(destDepth = cachedDepth and:[cachedColormap isColormap not]) 
		ifTrue: [^ cachedColormap].
	newMap := Bitmap new: colors size.
	1 to: colors size do: [:i |
		newMap
			at: i
			put: ((colors at: i) pixelValueForDepth: destDepth)].

	cachedDepth := destDepth.
	^ cachedColormap := newMap.
