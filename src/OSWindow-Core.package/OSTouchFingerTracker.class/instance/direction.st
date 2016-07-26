calculating
direction
	"Only returns noth, east, west, south"
	^ delta x abs > delta y abs
		ifTrue: [ delta x sign @ 0 ]
		ifFalse: [ 0 @ delta y sign ]