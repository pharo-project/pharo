printing
printOn: aStream 
	super printOn: aStream.
	self name isNil 
		ifFalse: [ aStream nextPutAll: ' name: '; print: self name ]