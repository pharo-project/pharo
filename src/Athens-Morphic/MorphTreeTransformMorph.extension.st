Extension { #name : 'MorphTreeTransformMorph' }

{ #category : '*Athens-Morphic' }
MorphTreeTransformMorph >> drawSubmorphsOnAthensCanvas: anAthensCanvas [

	| clip |
	clip := self clippingBounds intersect: anAthensCanvas clipRect.
	anAthensCanvas
		transformBy: self transform
		withClip: clip
		in: self innerBounds
		during: [ :canvas |
				| top bottom |
				top := self topVisibleRowForCanvas: canvas.
				bottom := self bottomVisibleRowForCanvas: canvas startingAt: top.
				bottom to: top by: -1 do: [ :row |
						| m |
						m := self submorphs basicAt: row.
						self drawRawColorOn: anAthensCanvas asCanvasWrapper forSubmorph: m.
						canvas fullDrawMorph: m ] ].
	self owner withTreeLines ifTrue: [ self owner drawLinesOn: anAthensCanvas asCanvasWrapper ].
	self owner enabled ifFalse: [
		anAthensCanvas asCanvasWrapper fillRectangle: self owner innerBounds fillStyle: (self owner paneColor alpha: 0.2) ]
]
