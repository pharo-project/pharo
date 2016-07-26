initialize-release
readFrom: stream
	| line lineStream httpCode |
	line := (ZnLineReader on: stream) nextLine.
	lineStream := line readStream.
	self version: (lineStream upTo: Character space).
	httpCode := lineStream upTo: Character space.
	httpCode := Integer readFrom: httpCode ifFail: [ (ZnUnknownHttpStatusCode code: httpCode) signal ].
	self code: httpCode.
	self reason: (lineStream upToEnd)
