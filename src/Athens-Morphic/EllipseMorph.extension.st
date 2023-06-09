Extension { #name : #EllipseMorph }

{ #category : #'*Athens-Morphic' }
EllipseMorph >> asAthensShapeOn: anAthensCanvas [
	| halfWidth halfHeight |
	self bounds area > 0
		ifFalse: [ ^ self ].
	halfWidth := self bounds width / 2 asFloat.
	halfHeight := self bounds height / 2 asFloat.
	^ anAthensCanvas
		createPath: [ :builder |
			builder
				absolute;
				moveTo: self bounds center;
				relative;
				moveTo: halfWidth negated @ 0;
				curveVia: 0 @ (halfHeight negated * 0.55)
					and: (0.45 * halfWidth) @ halfHeight negated
					to: halfWidth @ halfHeight negated;
				curveVia: (halfWidth * 0.55) @ 0 and: halfWidth @ (0.45 * halfHeight) to: halfWidth @ halfHeight;
				curveVia: 0 @ (halfHeight * 0.55) and: (0.45 * halfWidth negated) @ halfHeight to: halfWidth negated @ halfHeight;
				curveVia: (halfWidth negated * 0.55) @ 0
					and: halfWidth negated @ (halfHeight negated * 0.45)
					to: halfWidth negated @ halfHeight negated ]
]

{ #category : #'*Athens-Morphic' }
EllipseMorph >> drawOnAthensCanvas: anAthensCanvas [
	| border ellipse |
	self bounds area > 0
		ifFalse: [ ^ self ].
	border := self borderStyle.
	ellipse := self asAthensShapeOn: anAthensCanvas.
	anAthensCanvas setPaint: self fillStyle.
	anAthensCanvas setShape: ellipse.
	anAthensCanvas draw.
	(anAthensCanvas setStrokePaint: border color) width: border width.
	anAthensCanvas draw
]
