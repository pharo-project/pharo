drawing
gridFormOrigin: origin grid: smallGrid background: backColor line: lineColor

	| bigGrid gridForm gridOrigin |
	gridOrigin := origin \\ smallGrid.
	bigGrid := (smallGrid asPoint x) @ (smallGrid asPoint y).
	gridForm := Form extent: bigGrid depth: Display depth.
	backColor ifNotNil: [gridForm fillWithColor: backColor].
	gridOrigin x to: gridForm width by: smallGrid x do:
		[:x | gridForm fill: (x@0 extent: 1@gridForm height) fillColor: lineColor].
	gridOrigin y to: gridForm height by: smallGrid y do:
		[:y | gridForm fill: (0@y extent: gridForm width@1) fillColor: lineColor].
	^ InfiniteForm with: gridForm
