*Fuel
materializeFrom: aDecoder

	^ (self new: 2)
		at: 1 put: aDecoder nextEncodedUint32;
		at: 2 put: aDecoder nextEncodedUint32;
		yourself.
