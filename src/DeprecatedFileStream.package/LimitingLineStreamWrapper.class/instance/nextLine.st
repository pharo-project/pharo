accessing
nextLine

	| thisLine |
	self atEnd ifTrue: [^nil].
	thisLine := line.
	line := stream nextLine.
	^thisLine
