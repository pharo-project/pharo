private protocol
nextLineFromStream
	| line |
	line := lineReader nextLine.
	self logVerbose: line.
	^ line