*Fuel
materializeFrom: aDecoder
	
	^ self 
		seconds: aDecoder nextEncodedUint24
		nanoSeconds: aDecoder nextEncodedUint32