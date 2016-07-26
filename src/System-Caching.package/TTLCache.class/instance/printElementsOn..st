printing
printElementsOn: stream
	super printElementsOn: stream.
	stream space; print: timeToLive asDuration