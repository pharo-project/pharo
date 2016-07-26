writing
fileOut: aConfiguration on: aStream
	| inst |
	inst := self on: aStream.
	inst writeConfiguration: aConfiguration.
	inst close.
	
