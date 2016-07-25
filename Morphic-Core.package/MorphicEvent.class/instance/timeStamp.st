accessing
timeStamp
	"Return the millisecond clock value at which the event was generated"
	^timeStamp ifNil:[timeStamp := Time millisecondClockValue]