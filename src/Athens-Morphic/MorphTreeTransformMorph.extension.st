Extension { #name : #MorphTreeTransformMorph }

{ #category : #'*Athens-Morphic' }
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
				m := submorphs basicAt: row.
				self drawRawColorOn: anAthensCanvas asCanvasWrapper forSubmorph: m.
				canvas fullDrawMorph: m ] ].
	owner withTreeLines
		ifTrue: [ owner drawLinesOn: anAthensCanvas asCanvasWrapper ].
	owner enabled
		ifFalse: [ anAthensCanvas asCanvasWrapper fillRectangle: owner innerBounds fillStyle: (owner paneColor alpha: 0.2) ]
]
