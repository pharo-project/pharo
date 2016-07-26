testing
intersects: aRectangle 
	"Answer whether aRectangle intersects the receiver anywhere."
	"Optimized; old code answered:
		(origin max: aRectangle origin) < (corner min: aRectangle corner)"
	| rOrigin rCorner |
	rOrigin := aRectangle origin.
	rCorner := aRectangle corner.
	rCorner x <= origin x ifTrue: [ ^ false ].
	rCorner y <= origin y ifTrue: [ ^ false ].
	rOrigin x >= corner x ifTrue: [ ^ false ].
	rOrigin y >= corner y ifTrue: [ ^ false ].
	^ true