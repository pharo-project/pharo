printing
printOn: aStream
	super printOn: aStream.
	self isDevelopment 
		ifTrue: [ 
			aStream nextPutAll: ' #development'.
			^self ].
	self isStable
		ifTrue: [ aStream nextPutAll: ' #stable' ].