converting
asDateAndTime
	^ (DateAndTime fromUnixTime: self time) 
		offset: self offset;
		yourself