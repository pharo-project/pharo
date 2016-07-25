instruction decoding
jump: offset if: bool
	branchTargets at: self pc + offset put: currentPC