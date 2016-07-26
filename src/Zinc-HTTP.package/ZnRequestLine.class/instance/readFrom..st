initialize-release
readFrom: stream
	| line lineStream |
	line := (ZnLineReader on: stream) nextLine.
	lineStream := line readStream.
	self method: (lineStream upTo: Character space).
	self uri: (lineStream upTo: Character space).
	self version: (lineStream upToEnd)