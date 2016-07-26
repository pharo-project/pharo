printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Retrying '; print: exception