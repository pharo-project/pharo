printing
printOn: stream 
	super printOn: stream.
	stream << $( << self name << $)