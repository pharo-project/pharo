printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Request Read '; print: request; space; print: duration; << 'ms'