tests
testCopyDestExists
	| out |
	
	self 
		markForCleanup: 'gooly'; 
		markForCleanup: 'plonk'.
		
	out := (filesystem open: 'gooly' writable: true) writeStream.
	[out nextPutAll: 'gooly'] ensure: [out close].

	(filesystem open: 'plonk' writable: true) writeStream close.
	
	self 
		should: [filesystem copy: 'gooly' to: 'plonk']
		raise: FileExists