printing
printSeparateDateAndTimeOn: stream
	"Print the receiver as separate Date and Time to stream.
	See also #readSeparateDateAndTimeFrom:"

	stream
		print: self asDate;
		space;
		print: self asTime