printing
printOn: aStream 
	super printOn: aStream.
	submorphs isEmpty 
		ifTrue: [aStream nextPutAll: ' with no transformee!']
		ifFalse: [aStream nextPutAll: ' on ' , submorphs first printString]