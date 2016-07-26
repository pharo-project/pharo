accessing
placeFields: uuid
	"Fill the fields of uuid, effectively generating a new UUID in place, see my class comment"
	
	| byteStream |
	lock critical: [ 
		byteStream := uuid writeStream.
		self clockOn: byteStream.
		byteStream
			uint16: self nextCounter16;
			uint16: self nextRandom16;
			nextPutAll: node.
		self setVariantAndVersion: uuid ] 