printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Following Redirect '; print: target