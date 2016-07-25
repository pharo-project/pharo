accessing
newTempVector: aTempVector at: offset
	ownTempVectors add: aTempVector.
	self tempAt: offset