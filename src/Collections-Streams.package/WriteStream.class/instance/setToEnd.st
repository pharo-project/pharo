positioning
setToEnd 
	"Refer to the comment in PositionableStream|setToEnd."

	readLimit := readLimit max: position.
	super setToEnd.