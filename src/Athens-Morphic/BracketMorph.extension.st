Extension { #name : #BracketMorph }

{ #category : #'*Athens-Morphic' }
BracketMorph >> createClosedPolygonPathFrom: aPointsArray on: anAthensCanvas [
	^ anAthensCanvas
		createPath: [ :builder |
			aPointsArray ifEmpty: [ builder ].
			builder absolute.
			builder moveTo: aPointsArray first.
			aPointsArray allButFirstDo: [ :p | builder lineTo: p ].
			builder close.
			builder ]
]

{ #category : #'*Athens-Morphic' }
BracketMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Draw triangles at the edges."

	| r |
	r := self horizontal
		ifTrue: [ self bounds insetBy: (2 @ 1 corner: 2 @ 1) ]
		ifFalse: [ self bounds insetBy: (1 @ 2 corner: 1 @ 2) ].
	anAthensCanvas setPaint: self borderColor.
	anAthensCanvas drawShape: (self createClosedPolygonPathFrom:
	(self leftOrTopVertices: self bounds) on: anAthensCanvas).

	anAthensCanvas setPaint: self fillStyle.
	anAthensCanvas drawShape: (self createClosedPolygonPathFrom:
	(self leftOrTopVertices: r) on: anAthensCanvas).

	anAthensCanvas setPaint: self borderColor.
	anAthensCanvas drawShape: (self createClosedPolygonPathFrom:
	(self rightOrBottomVertices: self bounds) on: anAthensCanvas).

	anAthensCanvas setPaint: self fillStyle.
	anAthensCanvas drawShape: (self createClosedPolygonPathFrom:
	(self rightOrBottomVertices: r) on: anAthensCanvas)
]
