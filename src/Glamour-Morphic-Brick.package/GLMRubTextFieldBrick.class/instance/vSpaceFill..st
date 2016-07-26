brick-layout-properties
vSpaceFill: anInteger
	super vSpaceFill: anInteger.
	
	self isCentered ifTrue: [ self beCentered ]