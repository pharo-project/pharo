positioning
reset 
	"Refer to the comment in PositionableStream|reset."

	readLimit := readLimit max: position.
	position := 0