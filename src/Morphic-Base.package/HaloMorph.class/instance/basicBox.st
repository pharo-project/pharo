private
basicBox
	| aBox minSide anExtent w |
	minSide := 4 * self handleSize.
	anExtent := ((self width + self handleSize + 8) max: minSide) @
				((self height + self handleSize + 8) max: minSide).
	aBox := Rectangle center: self center extent: anExtent.
	w := self world ifNil:[target outermostWorldMorph].
	^ w
		ifNil:
			[aBox]
		ifNotNil:
			[aBox intersect: (w viewBox insetBy: 8@8) ifNone: [self error: 'should not happen' ]]
