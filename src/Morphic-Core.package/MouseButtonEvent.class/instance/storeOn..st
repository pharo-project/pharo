printing
storeOn: aStream
	super storeOn: aStream.
	aStream space.
	whichButton storeOn: aStream.