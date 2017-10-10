accessing
selectedLocation
	"Answer the location within the receiver of the selected colour
	relative to the receiver's top left."

	|b c x y|
	b := self innerBounds.
	c := self selectedColor.
	x := c saturation * (b width - 1).
	y := 1 - c brightness * (b height - 1).
	^(x truncated @ y truncated) + b topLeft