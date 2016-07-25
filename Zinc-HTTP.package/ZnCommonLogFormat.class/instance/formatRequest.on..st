private
formatRequest: event on: stream
	| requestLine |
	stream space.
	requestLine := event request requestLine.
	stream << $".
	requestLine printMethodAndUriOn: stream.
	stream space; << requestLine version.
	stream << $"