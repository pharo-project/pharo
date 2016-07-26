instruction decoding
popIntoTemporaryVariable: offset
	newTempVector
		ifNil: [
			self storeIntoTemporaryVariable: offset.
			self doPop. ]
		ifNotNil: [
			self scope newTempVector: newTempVector at: offset.
			" Keep offset for remapping in popScope "
			newTempVector index: offset.
			irBuilder createTempVectorNamed: newTempVector withVars: newTempVector.
			newTempVector := nil ]