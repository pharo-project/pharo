public api
scan: method
	loops := Array new: method endPC.
	branchTargets := Array new: method endPC.
	instructionStream := InstructionStream on: method.
	self interpretMethod: method.
	^ loops