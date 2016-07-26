private
setColors: colorArray cachedColormap: aBitmap depth: anInteger
	"Semi-private. Set the color array, cached colormap, and cached colormap depth to avoid having to recompute the colormap when switching color palettes in animations."

	colors := colorArray.
	cachedDepth := anInteger.
	cachedColormap := aBitmap.
