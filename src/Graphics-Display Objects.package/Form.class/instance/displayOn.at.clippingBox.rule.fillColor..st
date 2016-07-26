displaying
displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: rule fillColor: aForm

	aDisplayMedium copyBits: self boundingBox
		from: self
		at: aDisplayPoint + self offset
		clippingBox: clipRectangle
		rule: rule
		fillColor: aForm
		map: (self colormapIfNeededFor: aDisplayMedium).
