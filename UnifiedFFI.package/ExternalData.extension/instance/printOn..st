*UnifiedFFI
printOn: stream 
	stream << $( << type asString << $). 
	self getHandle printOn: stream.