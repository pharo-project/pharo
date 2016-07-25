opcode sizing
nextPut: aByte
	"For sizing make the encoder its own stream and
	 keep track of position with this version of nextPut:"
	position := position + 1