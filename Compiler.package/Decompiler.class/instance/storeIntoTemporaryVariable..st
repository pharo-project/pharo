instruction decoding
storeIntoTemporaryVariable: offset

	self pushTemporaryVariable: offset; doStore: stack