private
openConnectionToHost: host port: port
	| stream |
	stream := super openConnectionToHost: host port: port.
	self setReferenceSocketStreamOptions: stream.
	^ stream