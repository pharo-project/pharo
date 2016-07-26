printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Request Handled '; print: request; space; print: duration; << 'ms'