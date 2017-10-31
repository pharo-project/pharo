private 
globalSubscribe: aFileName

	| tsfo |
	
	tsfo := self forFileNamed: aFileName.
		
	Transcript addOutput: tsfo.
	
	