printing
printOn: aStream
	super printOn: aStream.
	self isBleedingEdge 
		ifTrue: [ aStream nextPutAll: ' #bleedingEdge' ].