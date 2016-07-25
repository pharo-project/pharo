private
openConnectionToHostNamed: host port: port
	| stream |
	stream := super openConnectionToHostNamed: host port: port.
	self setReferenceSocketStreamOptions: stream.
	^ stream