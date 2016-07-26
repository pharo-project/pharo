sources, change log
timeStamp: aStream
	"Writes system version and current time on stream aStream."

	| dateTime |
	dateTime := DateAndTime now.
	aStream
		nextPutAll: 'From ' , self datedVersion , ' [' , self lastUpdateString , '] on ' , dateTime asDate printString;
		nextPutAll: ' at ';
		nextPutAll: dateTime asTime printString