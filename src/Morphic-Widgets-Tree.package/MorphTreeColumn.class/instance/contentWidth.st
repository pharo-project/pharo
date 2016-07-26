column drawing
contentWidth
	| w |
	w := 0.
	self container allNodeMorphs do: [:n | w := w max: (n columnMorphAt: self index) width].
	^ w