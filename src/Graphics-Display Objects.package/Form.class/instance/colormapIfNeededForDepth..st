color mapping
colormapIfNeededForDepth: destDepth
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	self depth = destDepth ifTrue: [^ nil].  "not needed if depths are the same"
	^ Color colorMapIfNeededFrom: self depth to: destDepth
