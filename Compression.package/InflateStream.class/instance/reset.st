initialize
reset
	"Position zero - nothing decoded yet"
	position := readLimit := 0.
	sourcePos := 0.
	bitBuf := bitPos := 0.
	state := 0.