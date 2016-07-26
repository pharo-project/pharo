printing
storeOn: aStream
	super storeOn: aStream.
	aStream space.
	self startPoint x storeOn: aStream.
	aStream space.
	self startPoint y storeOn: aStream.
	aStream space.
	"trail storeOn: aStream."