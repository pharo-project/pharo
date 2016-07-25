as yet unclassified
updateGradients
	"Update the gradient directions."

	|bgm b|
	b := self innerBounds.
	bgm := self submorphs last.
	bgm bounds: b.
	bgm fillStyle
		origin: b topLeft;
		direction: 0@b height.
	self fillStyle
		origin: b topLeft;
		direction: (b width@0).
	self updateSelectedLocation