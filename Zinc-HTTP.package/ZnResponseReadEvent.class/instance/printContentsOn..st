printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Response Read '; print: response; space; print: duration; << 'ms'