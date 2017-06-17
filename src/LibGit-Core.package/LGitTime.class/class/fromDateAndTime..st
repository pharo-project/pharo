instance creation
fromDateAndTime: aDateAndTime
	^ self new 
		time: aDateAndTime asUnixTime;
		offset: aDateAndTime offset asMinutes asInteger;
		yourself