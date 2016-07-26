input
readSeparateDateAndTimeFrom: stream
	"Read a separate Date and Time from stream to instanciate the receiver.
	See also #printSeparateDateAndTimeOn:"
	
	| date time |
	stream skipSeparators.
	date := Date readFrom: stream.
	stream skipSeparators.
	time := Time readFrom: stream.
	^ self 
		date: date
		time: time