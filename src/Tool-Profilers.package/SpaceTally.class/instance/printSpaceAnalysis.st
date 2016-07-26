fileout
printSpaceAnalysis	
	| stream |
	stream := FileStream newFileNamed: 'STspace.text'.
	[ self printSpaceAnalysis: 1 on: stream ] ensure: [ stream close ]