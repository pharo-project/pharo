open/close
close
	"Close the serial port. Do nothing if the port is not open."
	
	port ifNotNil: [port isString 
					ifTrue: [self primClosePortByName: port]
 					ifFalse: [self primClosePort: port]].
	port := nil