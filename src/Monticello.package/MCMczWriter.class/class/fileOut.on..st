writing
fileOut: aVersion on: aStream
	| inst |
	inst := self on: aStream.
	inst writeVersion: aVersion.
	inst flush.
	
