printing
printOn: aStream
	
	super printOn: aStream.
	self allVisibleSlots printElementsOn: aStream.