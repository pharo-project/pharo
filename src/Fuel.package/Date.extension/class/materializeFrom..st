*Fuel
materializeFrom: aDecoder
	^ self 
		starting: (DateAndTime materializeFrom: aDecoder) midnight 
		duration: (Duration days: 1)