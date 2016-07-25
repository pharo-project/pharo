initialization
initializeText: aTxModel 
	layoutView := TxAthensLayoutView new.
	layoutView container: self.
	layoutView initWithPosition: aTxModel startPosition extent: self extent. 