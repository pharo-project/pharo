positioning
position: anInteger 
	"Refer to the comment in PositionableStream|position:."

	readLimit := readLimit max: position.
	super position: anInteger