private
brickBounds
	"brickBounds give direct access to the bounds Rectangle"
	^ bounds ifNil: [ bounds := 0@0 corner: 50@50 ]

	