scrolling
scrollUp: anInteger

	"preventing from scrolling if already at the top"
	band brickBounds origin x >= 0 ifTrue: [ ^ self ].
	"actuall scrolling"
	band brickBounds: (band brickBounds translateBy: (anInteger@ 0)).
	"returning scrollpane back if went away"
	band brickBounds origin x > 0 ifTrue: [ band brickBounds: (band brickBounds translateBy: (band brickBounds origin x@ 0 ) negated) ]