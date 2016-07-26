enumerating
collectWithIndex: aBlockClosure 
	
	^ mutex critical: [ value withIndexCollect: aBlockClosure ]