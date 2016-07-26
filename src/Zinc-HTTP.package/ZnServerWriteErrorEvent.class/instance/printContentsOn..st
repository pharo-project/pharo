printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Server Write Error ';  print: exception