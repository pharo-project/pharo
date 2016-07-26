A gradient fill style is a fill which interpolates smoothly between any number of colors.

Instance variables:
	colorRamp	<Array of: Association> Contains the colors and their relative positions along the fill, which is a number between zero and one.
	pixelRamp	<Bitmap>		A cached version of the colorRamp to avoid needless recomputations.
	radial		<Boolean>	If true, this fill describes a radial gradient. If false, it is a linear gradient.
	isTranslucent	<Boolean>	A (cached) flag determining if there are any translucent colors involved.

Class variables:
	PixelRampCache <LRUCache>	Recently used pixelRamps. They tend to have high temporal locality and this saves space and time.