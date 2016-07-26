instruction decoding
pushTemporaryVariable: offset

	stack addLast: (tempVars at: offset + 1)