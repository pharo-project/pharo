*Fuel
materializeFrom: aDecoder

	^ self basicNew
		fuelSet: aDecoder nextEncodedUint32
		nanoSecond: aDecoder nextEncodedUint32
		seconds: aDecoder nextEncodedInt24
		offset: (Duration 
			seconds: aDecoder nextEncodedInt24 
			nanoSeconds: aDecoder nextEncodedInt32);
		yourself.