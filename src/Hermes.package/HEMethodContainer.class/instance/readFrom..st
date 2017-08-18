reading
readFrom: aReader
	self doReadHeaderFrom: aReader.
	self doReadMethodsFrom: aReader