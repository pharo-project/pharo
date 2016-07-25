instance creation
crc32FromCollection: aCollection
	| instance |
	instance := 
		self new
			beCrc32;
			message: aCollection;
			yourself.
	
	^ instance runRefInRefOut