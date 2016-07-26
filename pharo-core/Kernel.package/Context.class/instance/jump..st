instruction decoding
jump: distance 
	"Simulate the action of a 'unconditional jump' bytecode whose offset is 
	the argument, distance."

	pc := pc + distance