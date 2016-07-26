printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Server Handler Error ';  print: exception