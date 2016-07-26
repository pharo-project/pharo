geometry
mapMouseRelativeToSlider: aPosition

	"Map mouse (screen position) relative to slider position"

	^ (aPosition x - bounds left) min: bounds width max: 0
