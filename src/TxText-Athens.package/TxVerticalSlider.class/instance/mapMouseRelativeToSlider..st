geometry
mapMouseRelativeToSlider: aPosition

	"Map mouse (screen position) relative to slider position"

	^ (aPosition y - bounds top) min: bounds height max: 0