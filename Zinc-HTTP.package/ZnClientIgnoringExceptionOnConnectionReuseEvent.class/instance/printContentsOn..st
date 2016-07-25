printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Ignoring Exception On Connection Reuse '; print: exception