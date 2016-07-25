private
moveInBufferDown
	"Move down contents of inBuffer to the start.
	Return distance moved."

	| sz distanceMoved |
	sz := inNextToWrite - lastRead - 1.
	inBuffer replaceFrom: 1 to: sz with: inBuffer startingAt: lastRead + 1.
	distanceMoved := lastRead.
	lastRead := 0.
	inNextToWrite := sz + 1.
	^distanceMoved
