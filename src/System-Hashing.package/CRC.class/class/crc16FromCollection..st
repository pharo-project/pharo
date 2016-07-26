instance creation
crc16FromCollection: aCollection
	| instance |
	instance := 
		self new
			beCrc16;
			message: aCollection;
			yourself.
			
	^ instance runRefInRefOut