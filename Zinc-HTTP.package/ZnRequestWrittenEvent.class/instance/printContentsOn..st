printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Request Written '; print: request; space; print: duration; << 'ms'