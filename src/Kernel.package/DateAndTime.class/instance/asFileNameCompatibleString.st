converting
asFileNameCompatibleString
	| stream |
	stream := String new writeStream.
	self printYMDOn: stream.
	stream << '.'.
	self printHMSWithDashesOn: stream.
	^ stream contents