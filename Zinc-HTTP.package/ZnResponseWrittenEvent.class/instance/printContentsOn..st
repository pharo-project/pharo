printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Response Written '; print: response; space; print: duration; << 'ms'