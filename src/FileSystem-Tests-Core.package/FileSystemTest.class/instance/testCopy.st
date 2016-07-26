tests
testCopy
	| out in contents |
	
	self
		markForCleanup: 'gooly';
		markForCleanup: 'plonk'.
		
	out := filesystem writeStreamOn: 'gooly'.
	[ out nextPutAll: 'gooly' ] ensure: [ out close ].
	filesystem copy: 'gooly' to: 'plonk'.
	
	in := filesystem readStreamOn: 'plonk'.
	contents := [ in contents asString ] ensure: [ in close ].
	self assert: contents equals: 'gooly'