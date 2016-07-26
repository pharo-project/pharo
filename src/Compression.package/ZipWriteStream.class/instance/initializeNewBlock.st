initialization
initializeNewBlock
	"Initialize the encoder for a new block of data"
	literalFreq atAllPut: 0.
	distanceFreq atAllPut: 0.
	literalFreq at: EndBlock+1 put: 1.
	litCount := 0.
	matchCount := 0.