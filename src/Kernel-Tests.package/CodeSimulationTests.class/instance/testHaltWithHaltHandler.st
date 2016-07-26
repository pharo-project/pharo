tests
testHaltWithHaltHandler
	self runSimulated: [[self methodWithHalt] on: Halt do: [:err |]]  