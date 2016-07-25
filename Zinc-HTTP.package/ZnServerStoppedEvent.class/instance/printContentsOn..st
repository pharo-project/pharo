printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Stopped '; << description